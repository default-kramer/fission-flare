#lang typed/racket

; A snapshot is the state of a single player known by the server.
; When the player sends a rq:sync, the server produces a new snapshot.
; (Or the server may keep the same snapshot and return an error response.)

(provide (struct-out snapshot) Snapshot EventBus
         handle-sync)

(require "../typed-utils.rkt"
         "data.rkt"
         "frame.rkt"
         )

(module queue typed/racket
  (provide EventBus dequeue)
  ; Small insulation layer to ensure that we have read-only access to the bus.
  ; (We never want to write to the bus in this file.)
  ; The bus contains events generated from all players.

  (require "bus.rkt" "data.rkt")
  (define-type EventBus (Busof (Stamped Event)))
  (: dequeue (-> EventBus (U #f (Pairof EventBus (Stamped Event)))))
  (define dequeue bus-dequeue))

(require (submod 'queue))

(struct snapshot ([id : SnapId]
                  [frame : Frame]
                  [homework : (Listof Action)]
                  [pid : Pid])
  #:transparent
  #:extra-constructor-name make-snapshot
  #:type-name Snapshot)

(: process-actions (-> Frame Pid (Listof (U (Stamped Action))) (Listof Action) (Listof (Stamped Event))
                       (U (Pairof Frame (Listof (Stamped Event)))
                          (Pairof #f Any))))
; Apply all actions to the given frame. Ensure every action succeeds.
; Ensure that all homework is completed.
(define (process-actions frame pid actions homework event-accum)
  (let* (#:break (when (empty? actions)
                   (if (empty? homework)
                       (cons frame event-accum)
                       (cons #f (list "Failed to complete homework" homework))))
         [action (car actions)]
         [stamp : Stamp (car action)]
         [framestamp (stamped-framestamp action)]
         #:break (when (not framestamp)
                   (cons #f (list "Missing framestamp" action)))
         [result (frame-fast-forward frame framestamp '())]
         #:break (when (not result)
                   (cons #f (list "Out-of-sequence framestamp" action frame)))
         [frame (car result)]
         [captured-events (cdr result)]
         [actions (cdr actions)]
         [action (stamped-value action)]
         [result (frame-apply-action frame action)]
         #:break (when (not result)
                   (cons #f (list "Failed to apply action:" action)))
         [frame (car result)]
         [captured-events (append captured-events (cdr result))]
         ; If the action we just did is at the front of the homework list, remove it.
         [homework (if (and (pair? homework)
                            (equal? action (car homework)))
                       (cdr homework)
                       homework)]
         ; We've already validated that the framestamp is accurate.
         ; Now we can add stamps and update the event-accum.
         [add-stamp : (-> Event (Stamped Event))
                    (lambda (e) (cons (make-stamp pid #f framestamp) e))]
         [new-events (cons (add-stamp (action-performed action))
                           (map add-stamp captured-events))]
         [event-accum (append event-accum new-events)])
    (process-actions frame pid actions homework event-accum)))

(: handle-sync (-> Snapshot rq:Sync EventBus
                   (values Snapshot
                           rs:Sync
                           (Listof (Stamped Event))
                           EventBus
                           )))
(define (handle-sync snap rq event-queue)
  (let* ([snap-id (snapshot-id snap)]
         #:break (when (not (= snap-id (rq:sync-from-snapshot-id rq)))
                   (fail "maybe send some kind of rs:reset here...?"))
         [pid (snapshot-pid snap)]
         [actions (rq:sync-actions rq)]
         [result (process-actions (snapshot-frame snap) pid actions (snapshot-homework snap) '())]
         [new-frame (car result)]
         #:break (when (not new-frame)
                   (fail "Failed to apply actions" (cdr result) actions))
         [events : (Listof (Stamped Event)) (cdr result)]
         [target-frame-count (rq:sync-frame-count rq)]
         ; Need to fast forward before we check that the hash codes match
         [result (frame-fast-forward new-frame target-frame-count '())]
         #:break (when (not result)
                   (fail "Target frame has already passed" target-frame-count frame))
         [new-frame (car result)]
         [events : (Listof (Stamped Event))
                 (append events (map (lambda ([e : Event])
                                       (cons (make-stamp pid #f target-frame-count) e))
                                     (cdr result)))]
         #:break (when (not (= (equal-hash-code new-frame)
                               (rq:sync-frame-hash-code rq)))
                   (fail "Client and server disagree on hash code"))
         [new-snap-id
          (add1 snap-id)]
         [(event-queue new-homework other-player-actions)
          (do-receive event-queue pid)]
         [new-snap
          (struct-copy snapshot snap
                       [id new-snap-id]
                       [frame new-frame]
                       [homework new-homework])])
    (values new-snap
            (rs:sync new-snap-id new-homework other-player-actions)
            events
            event-queue
            )))

(: do-receive (-> EventBus Pid (values EventBus (Listof Action) (Listof (Stamped Action)))))
; Reads all items from the bus.
; Returns the updated bus, a list of homework, and a list of actions performed by other players.
(define (do-receive queue pid)
  (: handle (-> EventBus (Listof Action) (Listof (Stamped Action))
                (values EventBus (Listof Action) (Listof (Stamped Action)))))
  (define (handle queue homework-accum other-player-actions-accum)
    (let* ([result (dequeue queue)]
           #:break (when (not result)
                     (values queue
                             (reverse homework-accum)
                             (reverse other-player-actions-accum)))
           [queue (car result)]
           [x (cdr result)])
      (cond
        [(let ([other-pid (stamped-pid x)])
           (and other-pid (not (= other-pid pid))))
         ; This Action -> Event -> Action feels a little goofy... but good enough for now
         (let ([e (stamped-value x)])
           (if (action-performed? e)
               (handle queue
                       homework-accum
                       (cons (cons (car x) (action-performed-action e))
                             other-player-actions-accum))
               (handle queue
                       (append (event->homework e) homework-accum)
                       other-player-actions-accum)))]
        [else
         (handle queue homework-accum other-player-actions-accum)])))
  (handle queue (list) (list)))

(: event->homework (-> Event (Listof Action)))
(define (event->homework e)
  (match e
    [(AttackGenerated amount)
     (list (make-take-damage amount))]
    [else empty]))
