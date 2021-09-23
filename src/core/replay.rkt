#lang typed/racket

(provide Replay replay? make-replay replay-frame
         replay-enqueue! replay-advance!)

(require "bus.rkt"
         "../typed-utils.rkt"
         "data.rkt"
         "frame.rkt")

(define-type Queue (Busof (Stamped Action)))

; A replay attempts to recreate the Frames implied by a series of Stamped Actions.
; If all the Stamped Actions are present immediately, the replay should have
; exactly the same timing as the original game.
; But in a live multiplayer game, Stamped Actions may arrive "too late"
; due to network lag or whatever.
; When this happens, a replay can try to gracefully catch up by skipping
; "unimportant" frames such as waiting frames.
;
; frame :
;   The most recent Frame.
;   We always advance this even if we have no actions in the queue.
; old-frame :
;   The result of the most recent action.
;   It only advances when an action is completed.
;   Because `frame` always advances, it might get a Stamped Action whose time
;   has already passed.
;   When that happens, we will use `old-frame` instead.
;   As long as actions arrive in order, this should never fail.
(struct replay ([frame : Frame]
                [old-frame : Frame]
                [action-queue : Queue])
  #:transparent
  #:mutable
  #:type-name Replay)

(: make-replay (-> Frame Replay))
(define (make-replay frame)
  (replay frame frame (make-bus)))

(: replay-enqueue! (-> Replay (U (Stamped Action) (Listof (Stamped Action))) Void))
(define (replay-enqueue! replay x)
  (let ([q (replay-action-queue replay)])
    (if (list? x)
        (for ([item x])
          (set! q (bus-enqueue! q item)))
        (set! q (bus-enqueue! q x)))
    (set-replay-action-queue! replay q)))

(: advance (-> Frame (Stamped Action) FrameCount
               ; The returned symbol answers "why wasn't the action performed?"
               ; 'ok : the action was performed
               ; 'action-too-new : the given frame could not be advanced far enough
               ; 'action-too-old : the action predates the given frame
               (List Frame (U 'ok 'action-too-old 'action-too-new))))
(define (advance frame sa target)
  (let* ([action-stamp (or (stamped-framestamp sa)
                           (fail "stamp lacks frame" sa))]
         [limit (min target (or action-stamp target))]
         [frame (frame-catch-up frame limit)]
         [counter (frame-counter frame)])
    (cond
      [(= counter action-stamp)
       (let* ([action (stamped-value sa)]
              [frame (or (frame-do-action frame action)
                         (fail "failed to do action" action frame))])
         (list frame 'ok))]
      [(< counter action-stamp)
       (list frame 'action-too-new)]
      [(> counter action-stamp)
       (list frame 'action-too-old)]
      [else
       (fail "unreachable")])))

(: replay-advance! (-> Replay FrameCount Void))
; Advance the replay to the target frame count (if possible).
(define (replay-advance! replay target)
  (define-syntax-rule (too-old? x)
    (equal? 'action-too-old (cadr x)))
  (let* ([frame (replay-frame replay)]
         #:break (when (>= (frame-counter frame) target)
                   (void))
         [state (frame-state frame)]
         #:break (when (state-game-over? state)
                   (void))
         [old-q (replay-action-queue replay)]
         [result (bus-dequeue old-q)]
         #:break (when (not result)
                   (set-replay-frame! replay (frame-catch-up frame target)))
         [new-q : Queue (car result)]
         [sa : (Stamped Action) (cdr result)]
         [result (advance frame sa target)]
         [result (if (too-old? result)
                     ; Back up to the known-good sync point and try again
                     (advance (replay-old-frame replay) sa target)
                     result)]
         #:break (when (too-old? result)
                   (fail "Stamped Action is too old" sa))
         [frame (car result)]
         [action-performed? : Boolean
                            (case (cadr result)
                              [(ok) #t]
                              [(action-too-old action-too-new) #f]
                              [else 'unreachable])])
    (set-replay-frame! replay frame)
    (when action-performed?
      (set-replay-old-frame! replay frame)
      (set-replay-action-queue! replay new-q))))
