#lang typed/racket

(provide single-player-game multi-player-game
         relaxed-replay-viewer relaxed-ai-stepper
         relaxed-single-player-game relaxed-multi-player-game)

(require typed/pict
         "../typed-utils.rkt"
         "../core.rkt")

(define-type Pict pict)

(define sync-delay 150) ; wait at least this many milliseconds between sync requests

(require/typed "graphics.rkt"
               [state->pict (-> State Any Any Pict)]
               [frame->pict (-> Frame Pict)]
               [cell-size (Parameterof Integer)])

; like dict-ref but only for lists
(: assoc-ref (All (K V) (-> (Listof (Pairof K V)) K V)))
(define (assoc-ref lst key)
  (cond
    [(empty? lst)
     (fail "key not found" key lst)]
    [(equal? key (car (car lst)))
     (cdr (car lst))]
    [else
     (assoc-ref (cdr lst) key)]))

; like dict-set but only for lists
(: assoc-set (All (K V) (-> (Listof (Pairof K V)) K V (Listof (Pairof K V)))))
(define (assoc-set lst key val)
  (cond
    [(empty? lst)
     (list (cons key val))]
    [(equal? key (car (car lst)))
     (cons (cons key val)
           (cdr lst))]
    [else
     (cons (car lst)
           (assoc-set (cdr lst) key val))]))

(with-controller-type Controller ([game-over? (-> Any)]
                                  [do-action! (-> Action Void)]
                                  [get-pict (-> Integer Integer Pict)]
                                  [next-frame! (-> Void)])

  (define (relaxed-replay-viewer [log : GameLog])
    (relax (replay-viewer log)))

  (define (relaxed-ai-stepper [frame : Frame] [port : (U #f Output-Port)])
    (relax (ai-stepper frame port)))

  (define (relaxed-single-player-game [x : (U State Frame)] [port : (U #f Output-Port)])
    (relax (single-player-game x port)))

  (define (relaxed-multi-player-game [all-players : (Listof PlayerInfo)]
                                     [pid : Pid]
                                     [sync-func : (-> rq:Sync (Evtof rs:Sync))])
    (relax (multi-player-game all-players pid sync-func))))

(: replay-viewer (-> GameLog Controller))
(define (replay-viewer gamelog)
  (define frame-num : Integer 0)
  (define replay : Replay (gamelog->replay gamelog))
  (lambda (sym)
    (case sym
      [(do-action!)
       (lambda (action)
         (void))]
      [(next-frame!)
       (lambda ()
         (set! frame-num (add1 frame-num))
         (replay-advance! replay frame-num)
         (void))]
      [(get-pict)
       (lambda (canvas-w canvas-h)
         (single-player-pict (replay-frame replay) canvas-w canvas-h))]
      [(game-over?)
       (lambda ()
         (state-game-over? (frame-state (replay-frame replay))))])))

(: basic-game (-> (Boxof Frame) (U #f Output-Port) Controller))
(define (basic-game frame-box record-port)

  (define wrote-last-frame? : Boolean #f)

  (define game-log : (-> Any Void)
    (if record-port
        (lambda (x)
          (write-dto x record-port)
          (displayln "" record-port))
        (lambda (x) (void))))

  (define-syntax (frame stx) #'(unbox frame-box))

  ; Also writes the last frame to the game log if we need to do so.
  (define (check-game-over)
    (let ([game-over? (state-game-over? (frame-state frame))])
      (when (and game-over? (not wrote-last-frame?))
        (set! wrote-last-frame? #t)
        (game-log `(#:last-frame ,frame)))
      game-over?))

  (game-log `(#:version 1))
  (game-log `(#:first-frame ,frame))

  (lambda (sym)
    (case sym
      [(do-action!)
       (lambda (action)
         (let ([new-frame (frame-do-action frame action)])
           (when new-frame
             (set-box! frame-box new-frame)
             (game-log (cons (frame-counter new-frame) action))
             (cons (make-stamp #f #f (frame-counter new-frame))
                   action))
           (void)))]
      [(next-frame!)
       (lambda ()
         (set-box! frame-box (car (next-frame frame)))
         (check-game-over)
         (void))]
      [(get-pict)
       (lambda (canvas-w canvas-h)
         (single-player-pict frame canvas-w canvas-h))]
      [(game-over?)
       check-game-over])))

(: single-player-game (-> (U State Frame) (U #f Output-Port) Controller))
(define (single-player-game x record-port)
  (let ([frame (if (frame? x)
                   x
                   (make-first-frame x))])
    (basic-game (box frame) record-port)))

(: ai-stepper (-> Frame (U #f Output-Port) Controller))
(define (ai-stepper first-frame record-port)
  (define frame-box (box first-frame))
  (define-syntax (frame stx) #'(unbox frame-box))

  (: translate-actions (-> (Listof Action) (Listof Action)))
  ; The AI output gives us plummet and burst per the State protocol, but
  ; we need to translate into drop-keydown/keyup for the Frame protocol.
  (define (translate-actions actions)
    (match actions
      [(list (action:plummet) (action:burst))
       ; hold the drop key down
       (list (action:drop-keydown))]
      [(list (action:plummet))
       ; press and release the drop key
       (list (action:drop-keydown) (action:drop-keyup))]
      [(list a)
       (fail "last action was expected to be plummet or plummet+burst")]
      [(list a b ...)
       (cons a (translate-actions b))]))

  (define pending-actions : (Listof Action) '())
  (define (make-plan!)
    (let* ([state (frame-state frame)]
           [plan (choose-move state)])
      (if plan
          (let* ([actions (translate-actions (cadr plan))]
                 ; always start the sequence by releasing the drop key
                 [actions (cons (action:drop-keyup) actions)])
            ;(pretty-print (print-grid (state-grid state)))
            (set! pending-actions actions))
          (fail "The only way to win is not to play...?"))))

  (let ([decorated (basic-game frame-box record-port)])
    (lambda (sym)
      (case sym
        [(do-action!)
         (lambda (action)
           (match action
             [(action:move 'r)
              (when (and (frame-waiting? frame)
                         (empty? pending-actions))
                (make-plan!))]
             [else #f])
           (void))]
        [(next-frame!)
         (lambda ()
           (when (and (not (empty? pending-actions))
                      (= 0 (modulo (frame-counter frame) 4)))
             (let ([action (car pending-actions)])
               (set! pending-actions (cdr pending-actions))
               ((decorated 'do-action!) action)))
           ((decorated 'next-frame!)))]
        [(get-pict)
         (decorated 'get-pict)]
        [(game-over?)
         (decorated 'game-over?)]))))

(: do-action (-> State Action (U #f State)))
(define (do-action state action)
  (car (state-apply state (list action))))

; Returns the scaled pict for a single-player game, with no padding
(: single-player-pict (-> Frame Integer Integer Pict))
(define (single-player-pict frame cw ch)
  (let* ([state (frame-state frame)]
         [h (state-height state)]
         [h (+ 2 h)] ; the clock at the bottom adds 2 rows
         [cell-height (quotient ch h)])
    (parameterize ([cell-size (min 40 cell-height)])
      (frame->pict frame))))

(: get-first-frame (-> PlayerInfo Frame))
(define (get-first-frame pi)
  (let* ([payload (or (player-info-start-game-payload pi)
                      (fail "Missing start payload"))]
         [settings : GameSettings (first payload)]
         [state (make-initial-state settings)])
    (make-first-frame state)))

(: multi-player-game (-> (Listof PlayerInfo) Pid (-> rq:Sync (Evtof rs:Sync)) Controller))
(define (multi-player-game all-players my-pid sync-func)

  (: find-player (-> Pid PlayerInfo))
  (define (find-player pid)
    (or (findf (lambda ([pi : PlayerInfo]) (equal? pid (player-info-pid pi)))
               all-players)
        (fail "player was not found" pid)))

  (define me : PlayerInfo
    (find-player my-pid))

  (define frame : Frame (get-first-frame me))

  (define other-players : (Listof PlayerInfo)
    (remove me all-players))

  (define current-snapshot-id : SnapId 0)

  ; The list of actions that have not yet been sent to the server
  (define current-snapshot-actions : (Listof (Stamped Action))
    (list))

  (: act! (-> Action Boolean))
  (define (act! action)
    (let ([new-frame (frame-do-action frame action)])
      (when new-frame
        (set! frame new-frame)
        (set! current-snapshot-actions
              (cons (add-stamp action new-frame) current-snapshot-actions)))
      (and new-frame #t)))

  (define replays : (Listof (Pairof Pid Replay))
    (map (lambda ([pi : PlayerInfo])
           (let* ([frame (get-first-frame pi)]
                  [replay (make-replay frame)])
             (cons (player-info-pid pi) replay)))
         other-players))

  (define the-evt : (U #f (Evtof rs:Sync))
    #f)

  (define last-sync : Flonum 0.0)

  (define (start-sync!)
    (when (not the-evt)
      (let* ([now (current-inexact-milliseconds)]
             [elapsed (- now last-sync)]
             [frame frame])
        (when (> elapsed sync-delay)
          (let* ([rq (rq:sync current-snapshot-id
                              (reverse current-snapshot-actions)
                              (frame-counter frame)
                              (equal-hash-code frame))])
            (set! last-sync now)
            (set! current-snapshot-actions (list))
            (set! the-evt (sync-func rq)))))))

  (define (finish-sync!)
    (let* ([x the-evt] ; x is not mutated
           [rs (and x (sync/timeout (ann 0 Nonnegative-Real) x))])
      (when rs
        (set! the-evt #f)
        (match rs
          [(rs:Sync new-snapshot-id homework other-player-actions)
           (set! current-snapshot-id new-snapshot-id)
           ; do homework immediately
           (for ([action homework])
             (or (act! action)
                 (fail "failed to do homework" action)))
           ; add other player actions to their own animation queue
           (for ([item other-player-actions])
             (let* ([pid (stamped-pid item)]
                    [replay (assoc-ref replays pid)])
               (replay-enqueue! replay item)))]))))

  (: add-stamp (-> Action Frame (Stamped Action)))
  (define (add-stamp action frame)
    (cons (make-stamp #f (current-inexact-milliseconds) (frame-counter frame)) action))

  (define opponent-pict-width : (U #f Real) #f)

  (: opponent-picts (-> (Listof (Pairof Pid Replay)) Real (Listof Pict)))
  (define (opponent-picts replays remaining-width)
    (match replays
      [(list) (list)]
      [(list item more-replays ...)
       (let* (#:break (when (< remaining-width (or opponent-pict-width 0))
                        (list))
              [pid (car item)]
              [replay (cdr item)]
              [frame (replay-frame replay)]
              [info (find-player pid)]
              [settings (player-info-settings info)]
              [name (player-settings-name settings)]
              [pict (parameterize ([cell-size 20])
                      (frame->pict frame))]
              [pict (vc-append (text name '(bold) 40) pict)]
              [consumed-width (pict-width pict)]
              [_ (set! opponent-pict-width consumed-width)]
              [remaining-width (- remaining-width consumed-width)]
              #:break (when (< remaining-width 0)
                        (list)))
         (cons pict (opponent-picts more-replays remaining-width)))]))

  (lambda (sym)
    (case sym
      [(do-action!)
       (lambda (action)
         (act! action)
         (void))]
      [(next-frame!)
       (lambda ()
         (set! frame (car (next-frame frame)))
         (let* ([counter (frame-counter frame)]
                ; Let's just see what happens with a 30-frame lag...
                ; TODO should detect and adjust this factor:
                [target (- counter 30)])
           (for ([item replays])
             (let* ([replay (cdr item)])
               (replay-advance! replay target))))
         (finish-sync!)
         (start-sync!))]
      [(get-pict)
       (lambda (canvas-w canvas-h)
         (define my-pict (single-player-pict frame canvas-w canvas-h))
         (define remaining-width (- canvas-w (pict-width my-pict)))
         (define other-picts : (Listof Pict)
           (opponent-picts replays remaining-width))
         (let ([picts : (Pairof Pict (Listof Pict))
                      (cons my-pict other-picts)])
           (apply hc-append picts)))]
      [(game-over?)
       (lambda ()
         ; TODO just local game over for now.
         ; Should wait for all players to finish (or time out) and show
         ; some multiplayer-specific results.
         (state-game-over? (frame-state frame)))])))
