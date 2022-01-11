#lang typed/racket

; This file makes the AI many games to completion, and verifies that each
; game is completed and that the average energy does not regress.
; This can take a long time to run, so we use places.

(module worker-impl typed/racket
  (provide play-game)

  (require "../core.rkt"
           "../typed-utils.rkt"
           (submod "ai.rkt" exercise-help)
           (only-in "ai.rkt" choose-move))

  (: play-game (-> Any Integer (U Integer (Listof Any))))
  ; Runs the AI to win the given game. Returns the final state's energy
  ; as an Integer, or returns a list of error information.
  (define (play-game settings-assoc max-moves)
    (: go (-> State Integer (U Integer (Listof Any))))
    (define (go state max-moves)
      (define-syntax-rule (on-error x ...)
        (list "play-game failed:" x ...))
      (let* (#:break (when (max-moves . <= . 0)
                       (on-error "move limit reached"))
             [state (fast-forward state)]
             [game-over? (state-game-over? state)]
             #:break (when game-over?
                       (case game-over?
                         [(win) (state-energy state)]
                         [else (on-error "game-lost:" game-over?)]))
             [choice (choose-move state)]
             #:break (when (not choice)
                       (on-error "failed to choose move"))
             [seq (second choice)]
             [result (state-apply state seq)]
             [new-state (car result)]
             #:break (when (not new-state)
                       (on-error "failed to apply move seq")))
        (go new-state (sub1 max-moves))))
    (let* ([settings (assoc->settings settings-assoc)]
           [state (make-initial-state settings)])
      (go state max-moves))))

(module worker-channel racket
  ; I'm not sure why this code needs to be untyped. Maybe a TR bug?
  (provide make-worker)

  (require (submod ".." worker-impl))

  #;(: make-worker (-> Place))
  ; Creates a place expecting a request message of the form
  #;(list max-move-count game-settings-assoc anything ...)
  ; The response message will be
  #;(cons **result** echoed-request-message)
  ; Where **result** is the Integer value of the energy of the winning state,
  ; or a list containing error information.
  ; Also, a request message of #f terminates the place
  (define (make-worker)
    (place
     ch
     (define (loop)
       (define request (place-channel-get ch))
       (when request
         (let* ([move-count (first request)]
                [settings-assoc (second request)]
                [result (list "play-game timed out")]
                [th (thread
                     (lambda args
                       (set! result (play-game settings-assoc move-count))))])
           (sync/timeout 60 th)
           (kill-thread th)
           (place-channel-put ch (cons result request))
           (loop))))
     (loop))))

; Can be run using `raco test -s slow-test [more ...]`
(module+ slow-test
  (require typed/racket/async-channel
           typed/rackunit
           (except-in "../typed-utils.rkt" fail))
  (require/typed (submod ".." worker-channel)
                 [make-worker (-> Place)])

  ; This channel acts as a queue for workers that become available.
  (define worker-queue (ann (make-async-channel)
                            (Async-Channelof Place-Channel)))

  ; Create workers and add them to the queue.
  ; Also hold onto them in this list so we can clean them up later.
  (define all-workers : (Listof Place)
    (for/list ([i (in-range 8)])
      (let ([worker (make-worker)])
        (async-channel-put worker-queue worker)
        worker)))

  (: start-work (-> Any (Evtof Any)))
  ; Blocks until there is an available worker.
  ; Then sends that worker the given request message and returns an evt whose
  ; sync result is the response message from the worker.
  (define (start-work request-msg)
    (let ([worker (async-channel-get worker-queue)] ; block until we get one
          [response-msg : Any #'unreachable-value])
      (wrap-evt (thread
                 (lambda ()
                   (place-channel-put worker request-msg)
                   (set! response-msg (place-channel-get worker))
                   ; add this worker back into the queue
                   (async-channel-put worker-queue worker)))
                (lambda args response-msg))))

  (: run-trials (-> Integer (-> Any Any) Integer Void))
  ; Runs the given number of trials and assert that the average energy meets or
  ; exceeds expectations.
  (define (run-trials count request-func min-avg-energy)
    (define layout-name (object-name request-func))
    (define work-queue (ann (make-async-channel)
                            (Async-Channelof (U #f (Evtof Any)))))

    (: enqueue-work (-> Void))
    (define (enqueue-work)
      ; Use a hard-coded seed to avoid bad luck breaking the test.
      ; (Some seeds will be legitimately more difficult than others.)
      (let ([prng (vector->pseudo-random-generator
                   '#(1970691962 3632932342 2858274971 363470546
                                 1753236688 2592668703))])
        (for ([i (in-range count)])
          (when (= 0 (remainder i 10))
            (println (format "started ~a of ~a runs, ~a"
                             i count layout-name)))
          (let* ([_ (random 1 prng)] ; advance PRNG
                 [rand-vec (pseudo-random-generator->vector prng)]
                 [request (request-func rand-vec)])
            (async-channel-put work-queue (start-work request))))
        ; send #f to signal "all done"
        (async-channel-put work-queue #f)))

    (thread (lambda args (enqueue-work)))

    (: consume-completed-work (-> Integer Integer))
    ; Returns total energy
    (define (consume-completed-work sum)
      (let* ([evt (async-channel-get work-queue)]
             #:break (when (not evt) sum)
             [result (sync evt)])
        (match result
          [(list energy echoed-request ...)
           #:when (exact-integer? energy)
           (consume-completed-work (+ sum energy))]
          [else
           (begin
             (println result)
             (fail "Trial failed, deducting energy")
             (consume-completed-work (- sum 100000)))])))

    (define total-energy (consume-completed-work 0))
    (println (format "completed ~a runs, ~a" count layout-name))
    (define avg-energy (truncate (/ total-energy count)))
    (let ([msg (format "~a average energy is ~a (expected >= ~a)"
                       layout-name avg-energy min-avg-energy)])
      (if (avg-energy . < . min-avg-energy)
          (fail msg)
          (println msg)))
    (void))

  (define (mini-layout-settings seed)
    (list 100 ; max-moves
          `((layout:mode . mini)
            (layout:fuel-height . 5)
            (layout:fuel-count . 12)
            (layout:num-waves . 1)
            (time-attack:type . #f)
            (time-attack:seconds . 600)
            (energy:initial . 10000)
            (energy:max . 10000)
            (energy:catalyst-cost . 55)
            (energy:drain-rate . 0)
            (energy:combo-payout . #(0 200 500 1000 2000))
            (energy:fuel-value . 100)
            (energy:horizontal-bonus-factor . 20)
            (energy:wave-completion-bonus . 5000)
            (penalty:type . #f)
            (penalty:resistance . 1000)
            (penalty:catalyst-cost . 0)
            (penalty:floor-drop-amount . 4)
            (attack:combo-payout . #(0 50 100 200 500 1000))
            (attack:horizontal-bonus-factor . 50)
            (misc:catalyst-deck . blank-12)
            (misc:combos-require-fuel? . #f)
            (misc:random-seed . ,seed))))

  (define (standard-layout-settings seed)
    (list 200 ; max-moves
          `((layout:mode . standard)
            (layout:fuel-height . 13)
            (layout:fuel-count . 84)
            (layout:num-waves . 1)
            (time-attack:type . #f)
            (time-attack:seconds . 600)
            (energy:initial . 40000)
            (energy:max . 40000)
            (energy:catalyst-cost . 250)
            (energy:drain-rate . 0)
            (energy:combo-payout . #(0 200 500 1000 2000))
            (energy:fuel-value . 100)
            (energy:horizontal-bonus-factor . 20)
            (energy:wave-completion-bonus . 5000)
            (penalty:type . #f)
            (penalty:resistance . 1000)
            (penalty:catalyst-cost . 0)
            (penalty:floor-drop-amount . 4)
            (attack:combo-payout . #(0 50 100 200 500 1000))
            (attack:horizontal-bonus-factor . 50)
            (misc:catalyst-deck . blank-12)
            (misc:combos-require-fuel? . #f)
            (misc:random-seed . ,seed))))

  (run-trials 1000 mini-layout-settings 10913)
  (run-trials 500 standard-layout-settings 27979)

  ; Clean up all the places
  (for ([worker all-workers])
    (place-channel-put worker #f)
    (when (not (sync/timeout 1.0 (place-dead-evt worker)))
      (println "place did not exit cleanly, killing...")
      (place-kill worker)))

  (println (format "~a, tests completed"
                   (syntax-source #'here)))
  )
