#lang typed/racket

(provide make-first-frame next-frame frame-do-action frame-apply-action
         frame-extra-occs frame-time-remaining frame-fast-forward frame-catch-up
         frame-waiting?
         )

(require "data.rkt" "state.rkt" "../typed-utils.rkt")

(define default-timing
  (let ([fps 30]
        [falling 3]
        [destroying 3]
        [bursting 15]
        [spawning 7])
    (make-timing fps falling destroying bursting spawning)))

; Almost every time we advance to a new frame, we want to keep track of any
; events that were generated. This will be a common return value:
(define-type Frame+Events (Pairof Frame (Listof Event)))

(: add-counter (-> FrameCount FrameInfo (Pairof FrameCount FrameInfo)))
(define add-counter cons)

(: make-first-frame (-> State Frame))
(define (make-first-frame state)
  (let* ([counter : FrameCount 0]
         ; There are probably a couple of initial values we could use for
         ; `info` here; '(falling) is one that works.
         [info (add-counter counter '(falling))])
    (frame state counter info default-timing)))

(: next-frame (-> Frame Frame+Events))
(define (next-frame frame)
  (let* ([counter (frame-counter frame)]
         [info (frame-info frame)]
         [kind (car (cdr info))]
         [timing (frame-timing frame)])
    (case kind
      [(game-over) (cons frame '())]
      [(bursting)
       (continue frame (timing-bursting timing) bursting->bursted)]
      [(destroying)
       (let* ([groups (get-groups frame)]
              #:break (when (not groups)
                        (fail "missing groups" frame))
              [delay (max-delay groups)]
              [frames-per-delay (timing-destroying timing)])
         (continue frame (* delay frames-per-delay) frame-tick))]
      [(falling)
       (continue frame (timing-falling timing) frame-tick)]
      [(spawning)
       (continue frame (timing-spawning timing) frame-tick)]
      [(waiting)
       (let* ([state (frame-state frame)]
              ; Check whether time attack time has expired.
              ; This can only happen during a waiting frame. In other words,
              ; we allow any falling/destroying that is in progress to complete.
              [time-remaining (frame-time-remaining frame)]
              #:break (when (and time-remaining
                                 (<= time-remaining 0))
                        (let ([state (struct-copy State state
                                                  [game-over? 'time-expired])])
                          (cons
                           (struct-copy Frame frame
                                        [state state]
                                        [info (add-counter counter '(game-over))])
                           '())))
              [settings (state-settings state)]
              [energy-cost (game-settings-energy:drain-rate settings)]
              [state (state-waiting-frame state energy-cost)]
              [counter (add1 counter)]
              [info (if (state-game-over? state)
                        (add-counter counter '(game-over))
                        info)])
         (cons
          (struct-copy Frame frame
                       [state state]
                       [info info]
                       [counter counter])
          '()))])))

(: continue (-> Frame FrameCount (-> Frame Frame+Events) Frame+Events))
(define (continue frame how-long transition)
  (let* ([count-now (frame-counter frame)]
         [info (frame-info frame)]
         [count-start (car info)]
         [deadline (+ count-start how-long)]
         [frame (struct-copy Frame frame
                             [counter (add1 count-now)])])
    (if (>= count-now deadline)
        (transition frame)
        (cons frame '()))))

(: bursting->bursted (-> Frame Frame+Events))
(define (bursting->bursted [frame : Frame])
  (let* ([state (frame-state frame)]
         [counter (frame-counter frame)]
         [new-info (add-counter counter '(falling))]
         [result (state-apply state (list (burst)))]
         #:break (when (not (car result))
                   ; Nothing bursted (no blanks on screen)
                   (list (struct-copy Frame frame
                                      [info new-info])))
         [state (car result)]
         [events (cdr result)])
    (cons
     (struct-copy Frame frame
                  [state state]
                  [info new-info])
     events)))

(: frame-time-remaining (-> Frame (U #f Real)))
; If this is a time attack game, return the number of seconds remaining
(define (frame-time-remaining frame)
  (let* ([counter (frame-counter frame)]
         [state (frame-state frame)]
         [settings (state-settings state)]
         [time-attack-type (game-settings-time-attack:type settings)]
         #:break (when (not time-attack-type)
                   #f)
         [elapsed-frames : Integer
                         (case time-attack-type
                           [(wall-clock) counter]
                           [(waiting-clock)
                            (stats-waiting-frames (state-stats state))])]
         [timing (frame-timing frame)]
         [fps (timing-fps timing)]
         [elapsed-time (/ elapsed-frames fps)]
         [time-limit (game-settings-time-attack:seconds settings)])
    (- time-limit elapsed-time)))

(define the-tick : Action (tick))

(: frame-tick (-> Frame Frame+Events))
(define (frame-tick frame)
  (: parse-events (-> (Listof Event) (Values (Listof CatalystSpawned)
                                             (Listof DestructionGroup))))
  (define (parse-events events)
    (for/fold ([spawn-events : (Listof CatalystSpawned) empty]
               [d-groups : (Listof DestructionGroup) empty])
              ([event events])
      (cond
        [(catalyst-spawned? event)
         (values (cons event spawn-events) d-groups)]
        [(occupants-destroyed? event)
         (values spawn-events (append (occupants-destroyed-groups event)
                                      d-groups))]
        [else
         (values spawn-events d-groups)])))
  (let* ([counter (frame-counter frame)]
         [counter (add1 counter)]
         [state (frame-state frame)]
         [result (state-apply state (list the-tick))]
         #:break (when (not (car result))
                   ; tick failed, which means we are now waiting
                   (cons
                    (struct-copy Frame frame
                                 [counter counter]
                                 [info (add-counter counter '(waiting))])
                    '()))
         [state : State (car result)]
         [events : (Listof Event) (cdr result)]
         [(spawn-events destruction-groups)
          (parse-events events)]
         [info : FrameInfo
               (cond
                 [(state-game-over? state)
                  '(game-over)]
                 [(pair? spawn-events)
                  (cons 'spawning spawn-events)]
                 [(pair? destruction-groups)
                  (cons 'destroying destruction-groups)]
                 [else
                  '(falling)])])
    (cons (struct-copy Frame frame
                       [state state]
                       [counter counter]
                       [info (add-counter counter info)])
          events)))

(: frame-do-action (-> Frame Action (U #f Frame)))
(define (frame-do-action frame action)
  (let ([result (frame-apply-action frame action)])
    (and result (car result))))

(: frame-apply-action (-> Frame Action (U #f Frame+Events)))
(define (frame-apply-action frame action)
  (let* ([state (frame-state frame)]
         [counter (frame-counter frame)]
         [info (frame-info frame)]
         [kind (cadr info)]
         #:break (when (drop-keyup? action)
                   (and (equal? 'bursting kind)
                        (cons (struct-copy Frame frame
                                           [info (add-counter counter '(falling))])
                              '())))
         [(action dropping?)
          (cond
            [(drop-keydown? action)
             (values (plummet) #t)]
            [else
             (values action #f)])]
         [result (state-apply state (list action))]
         [state (car result)]
         #:break (when (not state)
                   #f)
         [new-info : (U #f FrameInfo)
                   (cond
                     [(tick? action)
                      (fail "illegal argument - `next-frame` should generate ticks")]
                     [dropping?
                      '(bursting)]
                     [(or (plummet? action)
                          (burst? action))
                      '(falling)]
                     [else #f])])
    (cons (struct-copy Frame frame
                       [state state]
                       [info (if new-info
                                 (add-counter (frame-counter frame) new-info)
                                 (frame-info frame))])
          (cdr result))))

(: frame-extra-occs (-> Frame (HashTable Loc Occupant)))
; Used to animate the destruction ripple effect. Considers how many frames have
; elapsed and decides which recently-destroyed occupants should still be shown.
(define (frame-extra-occs frame)
  (let* ([groups (get-groups frame)]
         #:break (when (not groups)
                   (hash))
         [info (frame-info frame)]
         [counter (frame-counter frame)]
         [elapsed (- counter (car info))]
         [hash : (HashTable Loc Occupant)
               (make-hash)]
         [timing (frame-timing frame)]
         [frames-per-delay (timing-destroying timing)])
    (for ([group groups])
      (for ([item (destruction-group-items group)])
        (when (> (* frames-per-delay (destruction-animation-delay item))
                 elapsed)
          (hash-set! hash (destruction-loc item) (destruction-occ item)))))
    hash))

(: get-groups (-> Frame (U #f (Listof DestructionGroup))))
; If we are in the destroying phase, return the DestructionGroups.
; Otherwise return #f
(define (get-groups frame)
  (let* ([info (frame-info frame)]
         [kind (cadr info)])
    (and (equal? 'destroying kind)
         (cddr info))))

(: max-delay (-> (Listof DestructionGroup) Fixnum))
(define (max-delay groups)
  (let ([max-delay : Fixnum 0])
    (for ([group groups])
      (for ([item (destruction-group-items group)])
        (let ([delay (destruction-animation-delay item)])
          (when (> delay max-delay)
            (set! max-delay delay)))))
    max-delay))

(: frame-fast-forward (-> Frame FrameCount (Listof Event) (U #f Frame+Events)))
(define (frame-fast-forward frame target events)
  (let ([counter (frame-counter frame)])
    (cond
      [(= counter target)
       (cons frame events)]
      [(< counter target)
       (let ([result (next-frame frame)])
         (frame-fast-forward (car result)
                             target
                             (append events (cdr result))))]
      [else #f])))

(: frame-catch-up (-> Frame FrameCount Frame))
; Attempts to fast-forward to the target frame, but don't skip frames that are
; important for animation.
(define (frame-catch-up frame target)
  (let* ([counter (frame-counter frame)]
         #:break (when (>= counter target)
                   frame)
         [info (frame-info frame)]
         [kind (cadr info)])
    (case kind
      [(waiting)
       (frame-catch-up (car (next-frame frame)) target)]
      [(game-over)
       (struct-copy Frame frame
                    [counter target])]
      [(bursting destroying falling spawning)
       (car (next-frame frame))]
      [else 'unreachable])))

(: frame-waiting? (-> Frame Boolean))
(define (frame-waiting? [frame : Frame])
  (case (cadr (frame-info frame))
    [(waiting) #t]
    [(game-over bursting destroying falling spawning) #f]))
