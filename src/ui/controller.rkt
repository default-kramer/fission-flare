#lang typed/racket

(provide single-player-game multi-player-game
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

  (define (relaxed-single-player-game [x : (U State Frame)])
    (relax (single-player-game x)))

  (define (relaxed-multi-player-game [all-players : (Listof PlayerInfo)]
                                     [pid : Pid]
                                     [sync-func : (-> rq:Sync (Evtof rs:Sync))])
    (relax (multi-player-game all-players pid sync-func))))

(: single-player-game (-> (U State Frame) Controller))
(define (single-player-game x)
  (let ([frame (if (frame? x)
                   x
                   (make-first-frame x))])
    (lambda (sym)
      (case sym
        [(do-action!)
         (lambda (action)
           (set! frame (or (frame-do-action frame action)
                           frame)))]
        [(next-frame!)
         (lambda ()
           (set! frame (car (next-frame frame))))]
        [(get-pict)
         (lambda (canvas-w canvas-h)
           (single-player-pict frame canvas-w canvas-h))]
        [(game-over?)
         (lambda ()
           (state-game-over? (frame-state frame)))]))))

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
