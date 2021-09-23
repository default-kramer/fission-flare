#lang typed/racket

(provide make-server
         server-start
         server-start-2
         server-stop
         server-begin-game
         server-set-mp-settings
         server-player-infos
         merge-settings
         )

(require typed/racket/random
         typed/file/sha1
         "../typed-utils.rkt"
         "bus.rkt"
         "data.rkt"
         "state.rkt"
         "frame.rkt"
         "snapshot.rkt"
         )

(module+ test
  (require typed/rackunit))

; A client sends an rq? on the input port and the server responds with an rs? on the output port.
(struct connection ([rq-in : Input-Port]
                    [rs-out : Output-Port])
  #:type-name Connection
  #:transparent)

; The server-state (abbreviated `ss`) is the state of just one multiplayer game.
; Prefer boxes over #:mutable fields so that we can use box-cas! where needed.
(struct server-state
  ([mp-settings : (Boxof MultiplayerSettings)]
   [next-pid : (Boxof Pid)]
   [clients : (Boxof (Immutable-HashTable Pid (Boxof ClientState)))]
   ; When the game starts, we will copy mp-settings into the following box.
   ; This will become the official settings for the game.
   [game-started-signal : (Boxof (U #f MultiplayerSettings))]
   ; Communication from one player to another goes through the event bus.
   ; For example, when one player generates an attack they can write an AttackGenerated
   ; event onto the bus and every other player will eventually read it.
   [event-bus : (Boxof EventBus)]
   [shutdown-signal : (Boxof Boolean)]
   [custodian : (Boxof (U 'custodian-not-created Custodian 'custodian-destroyed))])
  #:type-name ServerState
  #:transparent)

(define (new-server-state)
  (server-state (box default-multiplayer-settings)
                (box 1)
                (box (ann (hash) (Immutable-HashTable Pid (Boxof ClientState))))
                (box #f)
                (box (ann (make-bus) EventBus))
                (box #f)
                (box 'custodian-not-created)))

(define-type Timestamp Flonum)
(define current-timestamp : (-> Timestamp)
  current-inexact-milliseconds)

; The client-state (abbreviated `cs`) is immutable.
; Handling a client's request typically works like this:
; * find the cs belonging to the client
; * call a pure-enough function that returns a new cs
; * replace the old cs with the new cs
; This design should almost (or completely?) eliminate box-cas! failures
; while handling a client request.
(struct client-state ([pid : Pid]
                      [token : Token]
                      [last-timestamp : Timestamp]
                      [event-bus : EventBus]
                      [player-info : PlayerInfo]
                      [snapshot : (U #f Snapshot)])
  #:type-name ClientState
  #:transparent)

(: ss-next-pid! (-> ServerState Pid))
; Returns a unique PID to be assigned to a new client.
(define (ss-next-pid! ss)
  (let* ([pid-box (server-state-next-pid ss)]
         [pid (unbox pid-box)])
    (if (box-cas! pid-box pid (add1 pid))
        pid
        (ss-next-pid! ss))))

(: ss-client-state (-> ServerState Pid (U #f ClientState)))
; Finds the cs for the given PID.
(define (ss-client-state ss pid)
  (let* ([dict (unbox (server-state-clients ss))]
         [cs (hash-ref dict pid #f)])
    (and cs (unbox cs))))

(: ss-shutdown? (-> ServerState Boolean))
(define (ss-shutdown? ss)
  (unbox (server-state-shutdown-signal ss)))

(: ss-shutdown! (-> ServerState Void))
(define (ss-shutdown! ss)
  (set-box! (server-state-shutdown-signal ss) #t)
  (let* ([cust-box (server-state-custodian ss)]
         [cust (unbox cust-box)])
    (when (and (custodian? cust)
               (box-cas! cust-box cust 'custodian-destroyed))
      (custodian-shutdown-all cust))))

(: ss-game-started? (-> ServerState Boolean))
(define (ss-game-started? ss)
  (and (unbox (server-state-game-started-signal ss)) #t))

(: merge-settings (-> MultiplayerSettings GameSettings GameSettings))
; Override client settings as needed to meet the server's constraints.
; This function is also used client-side to calculate and display the
; settings that the player will get when the game starts.
(define (merge-settings server-data client-data)

  (: can-modify-setting? (-> CompetitionType Any Boolean))
  (define (can-modify-setting? competition-type field-name)
    (case competition-type
      [(symmetric) #f]
      [(custom) #t]
      [(handicap)
       (case field-name
         [(penalty:resistance
           attack:combo-payout
           attack:horizontal-bonus-factor) #t]
         [else #f])]))

  (: go (-> CompetitionType Any Any Any))
  ; Given the two assoc lists, return a single assoc list
  (define (go ct server-assoc client-assoc)
    (match (list server-assoc client-assoc)
      [(list (list (cons field-name server-val) server-more ...)
             (list (cons field-name client-val) client-more ...))
       (let* ([val (if (can-modify-setting? ct field-name)
                       client-val
                       server-val)])
         (cons (cons field-name val) (go ct server-more client-more)))]
      [(list (list) (list))
       (list)]
      [else (fail "incomplete match" server-assoc client-assoc)]))

  (let* ([server-gs (multiplayer-settings-game-settings server-data)]
         [competition-type (multiplayer-settings-competition-type server-data)]
         [server-assoc (settings->assoc server-gs)]
         [client-assoc (settings->assoc client-data)]
         [client-assoc (go competition-type server-assoc client-assoc)])
    (assoc->settings client-assoc)))

(module+ test
  (let* ([server-gs default-game-settings]
         [mp-settings (make-multiplayer-settings server-gs 'speed 'handicap #f)])
    (define-syntax-rule (allowed? changes ...)
      (let* ([client-gs (struct-copy GameSettings server-gs
                                     changes ...)])
        (equal? client-gs
                (merge-settings mp-settings client-gs))))
    ; All of these settings can be modified (in handicap mode)
    (check-true (allowed? [penalty:resistance 333]
                          [attack:combo-payout #(2 3 12 40)]
                          [attack:horizontal-bonus-factor 321]))
    ; Just test a single setting that cannot be modified (in handicap mode)
    (check-false (allowed? [time-attack:seconds 333]))))

(: ensure-seed (-> MultiplayerSettings MultiplayerSettings))
; Adds a seed if none present.
(define (ensure-seed mp-settings)
  (let* ([game-settings (multiplayer-settings-game-settings mp-settings)]
         #:break (when (game-settings-misc:random-seed game-settings)
                   ; already seeded
                   mp-settings)
         [prng (make-pseudo-random-generator)]
         [seed (prng->vector prng)]
         [game-settings (struct-copy GameSettings game-settings
                                     [misc:random-seed seed])])
    (struct-copy MultiplayerSettings mp-settings
                 [game-settings game-settings])))

(: ss-start-game! (-> ServerState Void))
(define (ss-start-game! ss)
  (let* ([start-box (server-state-game-started-signal ss)]
         #:break (when (unbox start-box)
                   (void)) ; game already started
         [mp-settings (ss-mp-settings ss)]
         [mp-settings (ensure-seed mp-settings)]
         #:break (when (not (box-cas! start-box #f mp-settings))
                   (void)) ; game already started
         [hash (unbox (server-state-clients ss))]
         [player-count (hash-count hash)]
         [opponent-count (if (> player-count 1)
                             (sub1 player-count)
                             1)])
    (for ([key (hash-keys hash)])
      (define (try)
        (let* ([cs-box (hash-ref hash key)]
               [orig-cs (unbox cs-box)]
               [cs orig-cs]
               [player-info (client-state-player-info cs)]
               [player-settings (player-info-settings player-info)]
               [game-settings (player-settings-game-settings player-settings)]
               [game-settings (merge-settings mp-settings game-settings)]
               [resistance (game-settings-penalty:resistance game-settings)]
               [game-settings
                (struct-copy GameSettings game-settings
                             [penalty:resistance (* opponent-count resistance)])]
               [state (make-initial-state game-settings)]
               [player-info (struct-copy PlayerInfo player-info
                                         [start-game-payload (list game-settings mp-settings)])]
               [pid (client-state-pid cs)]
               [frame (make-first-frame state)]
               ; TODO does the first SnapId matter?
               [snapshot (make-snapshot 0 frame (list) pid)]
               [cs (struct-copy client-state cs
                                [snapshot snapshot]
                                [player-info player-info])])
          (box-cas! cs-box orig-cs cs)))
      (or (try) (try) (try)
          (fail "Is there a deadlock trying to start the game??")))))

(: ss-bus-cleanup! (-> ServerState Void))
; Discard all events from the bus.
; Implemented by advancing the bus as far as possible.
; This is done so that the server doesn't prevent the entire list from being GC'd
; by holding a reference to the first node.
; (A better approach might be to have the server not hold a reference to the bus at all.
;  When the first client is created, create a new bus then.
;  When a second client is created, use the first client's bus, fast-forwarded.
;  ... But that approach sounds tricky to synchronize.)
(define (ss-bus-cleanup! ss)
  (let* ([bus-box (server-state-event-bus ss)]
         [orig-bus (unbox bus-box)]
         [new-bus (bus-discard-all orig-bus)])
    (or (and (box-cas! bus-box orig-bus new-bus) (void))
        (ss-bus-cleanup! ss))))

(: ss-client-cleanup! (-> ServerState Timestamp Void))
; Remove all clients who haven't been seen since the given timetamp.
(define (ss-client-cleanup! ss timestamp)
  (: try (-> Boolean))
  (define (try)
    (let* ([clients-box (server-state-clients ss)]
           [clients (unbox clients-box)]
           [new-clients : (Immutable-HashTable Pid (Boxof ClientState))
                        clients]
           [keys (hash-keys clients)])
      (for ([key keys])
        (let* ([cs (unbox (hash-ref clients key))]
               [last-seen (client-state-last-timestamp cs)])
          (when (< last-seen timestamp)
            (set! new-clients (hash-remove clients key)))))
      (box-cas! clients-box clients new-clients)))
  ; Don't remove anyone once the game starts
  (when (not (ss-game-started? ss))
    (or (try) (try) (try)
        (fail "deadlock during client cleanup")))
  (void))

(: ss-update-client! (-> ServerState ClientState Void))
(define (ss-update-client! ss cs)
  (define-syntax-rule (cas! args ...)
    (or (and (box-cas! args ...) (void))
        (ss-update-client! ss cs)))
  (let* ([pid (client-state-pid cs)]
         [hash-box (server-state-clients ss)]
         [hash (unbox hash-box)]
         [my-box (hash-ref hash pid #f)]
         #:break (when (not my-box)
                   (let ([new-hash (hash-set hash pid (box cs))])
                     (cas! hash-box hash new-hash)))
         [old-cs (unbox my-box)])
    (cas! my-box old-cs cs)))

(: ss-player-infos (-> ServerState (Listof PlayerInfo)))
(define (ss-player-infos ss)
  (define (get-it [x : (Boxof ClientState)])
    (client-state-player-info (unbox x)))
  (let* ([clients (unbox (server-state-clients ss))]
         [clients (hash-values clients)])
    (map get-it clients)))

(: ss-mp-settings (-> ServerState MultiplayerSettings))
(define (ss-mp-settings ss)
  ; If present, the `game-started-signal` takes priority.
  ; This is the version of `mp-settings` that the game was started with.
  (or (unbox (server-state-game-started-signal ss))
      (unbox (server-state-mp-settings ss))))

; This exception is for errors that are due to a bad client or a bad network
; as opposed to those due to a server-side bug.
(struct exn:request-failed exn () #:transparent)

(: client-error (->* (String) #:rest Any Nothing))
(define (client-error msg . more)
  (let* ([formatter (error-value->string-handler)]
         [more (map (lambda (e) (formatter e 500)) more)]
         [message (string-join (cons msg more) " ")])
    (raise (exn:request-failed message (current-continuation-marks)))))

(: handle-rq (-> (U #f Pid) ServerState Request (values ClientState Response)))
(define (handle-rq pid ss rq)
  (define (get-cs [pid : (U #f Pid)])
    (assert pid)
    (or (ss-client-state ss pid)
        (client-error "Missing client state for PID:" pid)))
  (match rq
    [(rq:get-pid)
     (when pid
       (client-error "Already have a PID, cannot get a new one"))
     (let* ([pid (ss-next-pid! ss)]
            [secret (bytes->hex-string (crypto-random-bytes 80))]
            [token (make-token pid secret)]
            [timestamp (current-timestamp)]
            [game-started? (ss-game-started? ss)]
            #:break (when game-started?
                      (client-error "game has already started"))
            [bus (unbox (server-state-event-bus ss))])
       (values (client-state pid token timestamp bus (make-empty-player-info pid) #f)
               (rs:got-pid token)))]
    [(rq:become-pid token)
     (when pid
       (client-error "Already have a PID, cannot become a different one"))
     (let* ([pid (token-pid token)]
            [cs (get-cs pid)]
            [existing-token (client-state-token cs)]
            #:break (when (not (equal? token existing-token))
                      (client-error "Client's token does not match server's copy")))
       (values cs (rs:got-pid token)))]
    [(rq:poll-lobby settings)
     (let* ([cs (get-cs pid)]
            [infos (ss-player-infos ss)]
            [mps (ss-mp-settings ss)]
            [game-started? (ss-game-started? ss)]
            #:break (when game-started?
                      (values cs (rs:poll-lobby infos mps)))
            [pi (client-state-player-info cs)]
            [pi (struct-copy player-info pi
                             [settings settings])]
            [cs (struct-copy client-state cs
                             [player-info pi])])
       (values cs (rs:poll-lobby infos mps)))]
    [(rq:sync _a _b _c _d)
     (let* ([cs (get-cs pid)]
            [snapshot (client-state-snapshot cs)]
            #:break (when (not snapshot)
                      (client-error "game has not started"))
            [bus (client-state-event-bus cs)]
            [(snapshot rs events bus)
             (handle-sync snapshot rq bus)]
            [cs (struct-copy client-state cs
                             [event-bus bus]
                             [snapshot snapshot])])
       ; write-events!
       (for ([e events])
         (bus-enqueue! bus e))
       ; return values
       (values cs rs))]
    [else
     (client-error "unexpected rq:" rq)]))

; the type of Evt returned by tcp-accept-evt
(define-type AcceptEvt (Evtof (List Input-Port Output-Port)))

(struct server ([ss : ServerState])
  #:property prop:authentic #t
  #:type-name Server
  #:transparent)

(define (make-server)
  (server (new-server-state)))

(: server-start (->* (Server AcceptEvt) (#:custodian (U #f Custodian)) Void))
; Infinite loop that accepts client connections -- caller should probably use
; a dedicated thread for this.
; If a custodian is given to server-start, it will be shutdown by server-stop.
; (Otherwise we create our own child custodian and use that.)
(define (server-start server accept-evt #:custodian [cust #f])
  (let* ([ss (server-ss server)]
         [cust (or cust (make-custodian))]
         [cust-box (server-state-custodian ss)]
         #:break (when (not (box-cas! cust-box 'custodian-not-created cust))
                   (fail "server is already started")))
    (parameterize ([current-custodian cust])
      (server-loop accept-evt ss))))

(: server-start-2 (->* (Server TCP-Listener) (#:custodian (U #f Custodian)) Void))
(define (server-start-2 server listener #:custodian [cust #f])
  (server-start server (tcp-accept-evt listener) #:custodian cust))

(: server-stop (-> Server Void))
(define (server-stop server)
  (ss-shutdown! (server-ss server)))

(: server-set-mp-settings (-> Server MultiplayerSettings Void))
(define (server-set-mp-settings server mps)
  (let* ([ss (server-ss server)]
         [the-box (server-state-mp-settings ss)])
    (set-box! the-box mps)))

(: server-begin-game (-> Server Void))
(define (server-begin-game server)
  (ss-start-game! (server-ss server)))

(: server-loop (-> AcceptEvt ServerState Void))
(define (server-loop accept-evt ss)
  (define quit? : Boolean #f)
  (define (handle-fail [e : exn:fail])
    ; If the custodian was shut down, we want to exit without making a fuss.
    ; Otherwise re-raise the exception.
    (let ([msg (exn-message e)])
      (if (string-contains? msg "current custodian has been shut down")
          (begin (set! quit? #t)
                 #f)
          (raise e))))
  (define (accept-one)
    (let* ([result (with-handlers ([exn:fail? handle-fail])
                     (sync/timeout 1 accept-evt))]
           #:break (when (not result) #f)
           [conn (connection (first result) (second result))])
      (thread (lambda () (client-loop ss conn)))
      (void)))
  (: loop (-> Void))
  (define (loop)
    (if (or quit? (ss-shutdown? ss))
        (void)
        (begin (accept-one)
               (ss-bus-cleanup! ss)
               ; 3-second timeout for abandoned clients
               (ss-client-cleanup! ss (- (current-timestamp) 3000))
               (loop))))
  (loop))

(: server-player-infos (-> Server (Listof PlayerInfo)))
(define (server-player-infos server)
  (let* ([ss (server-ss server)]
         [clients (unbox (server-state-clients ss))]
         [pids (hash-keys clients)])
    (for/list ([pid pids])
      (let* ([cs (or (hash-ref clients pid #f)
                     (fail "This immutable hash lookup should never fail"))]
             [cs (unbox cs)])
        (client-state-player-info cs)))))

(: client-loop (-> ServerState Connection Any))
; Read a DTO from the client connection and handle it.
; Loop forever, because `read-dto` is not an Evt (so `sync/timeout` cannot be used).
; We will rely on the custodian to shut this loop down.
(define (client-loop ss conn)

  ; A fresh connection needs to establish a PID, but after that it must never change.
  (define pid : (U #f Pid) #f)

  (: handle-one (-> Boolean))
  (define (handle-one)
    (let* ([msg (read-dto (connection-rq-in conn))]
           #:break (when (eof-object? msg)
                     ; client disconnected
                     #f)
           #:break (when (not (rq? msg))
                     (client-error "invalid message (not an rq)" msg))
           [(cs rs)
            (handle-rq pid ss msg)]
           [cs (struct-copy client-state cs
                            [last-timestamp (current-timestamp)])]
           [result-pid (client-state-pid cs)])
      (when (not pid)
        ; Log: "connection <guid> now has PID <pid>"
        (set! pid result-pid))
      (when (not (equal? pid result-pid))
        (fail "PID should never mismatch here!" pid result-pid))
      (ss-update-client! ss cs)
      (let ([out (connection-rs-out conn)])
        (write-dto rs out))
      #t))

  (: loop (-> Any))
  (define (loop)
    ; We could immediately remove the player when `handle-one` returns false,
    ; but it's easier to just let them time out. This also gives the client a
    ; chance to reconnect using the existing Token.
    (when (handle-one)
      (loop)))

  (loop))

(define (make-empty-player-info [pid : Pid])
  (let* ([name "<name>"]
         [ready? #f]
         [settings (make-player-settings name ready? default-game-settings)]
         [state #f])
    (player-info pid settings state)))
