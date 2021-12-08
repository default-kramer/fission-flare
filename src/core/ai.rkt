#lang typed/racket

(provide grid-score choose-move)

(require racket/fixnum
         "../typed-utils.rkt"
         "data.rkt"
         "grid.rkt"
         "state.rkt")

; Two different classical (non-NN) approaches.
; 1. Figure out what to do given the current state and catalyst queue
; 2. Develop a plan regardless of the catalyst queue.

; Example of a plan-based approach:
; Given this grid
(define ex1
  (parse-grid
   '([RR BB .. .. .. .. .. ..]
     [.. .. .. .. BB RR .. ..]
     [RR RR .. .. .. YY YY ..]
     [RR .. .. YY .. .. .. ..]
     [BB .. .. .. .. .. BB ..])))
; Priority one will be clearing the [BB RR]
; (because it is covering yellow, which we are low on).
; Also notice that the [RR RR] can be cleared horizontally with no dependencies.
; So let's just assume we have a way to identify those two as the key ideas.
; This leads us to the next question of "how do we want to achieve that?"


; The Latency of a group is "what needs to happen for this group to reify?"
;   'immediate - nothing, the group is already the board
;   'burst - the group will reify after a burst operation
;   'shatter - the group will reify after all catalysts are split
; The "shatter" operation never happens in a game; it is a simple way to give us an
; idea of what might happen in the future. (The assumption is that all joined
; catalysts will eventually be separated after enough moves.)
(define-type Latency (U 'immediate 'burst 'shatter))

(struct group-info ([dg : DestructionGroup]
                    [latency : Latency])
  #:type-name GroupInfo
  #:transparent)

; How do we want to model this?
; Fuel stays at the same loc, so that part is easy.
; Model it as a list or a lookup of Loc -> GroupInfo


(: grid-resolve (-> Grid (Listof DestructionGroup)
                    (Values Grid (Listof DestructionGroup))))
; Fast-forward the "Fall -> Destroy -> Fall -> Destroy ..." cycle.
; Return resulting grid and all destruction groups.
(define (grid-resolve grid accum)
  (let* ([g2 (grid-fall grid)]
         #:break (when g2
                   (grid-resolve g2 accum))
         [(g2 groups)
          (grid-destroy grid)])
    (if g2
        (grid-resolve g2 (append groups accum))
        (values grid accum))))

(: destroy (-> Grid Integer (Listof DestructionGroup)))
; Surprise: Groups of 1 will get counted twice (horizontally and vertically),
; but this is OK because we only care about the best result.
; For example, if you are in a vertical group of 3 and a horizontal group of 2,
; your value is based on the group of 3 because that is better.
(define (destroy grid group-size)
  (let* ([(_ groups)
          (grid-destroy grid group-size)])
    groups))

(define-type Run (Listof (Pairof Loc Occupant)))

(: grid-runs (-> Grid Boolean (Listof Run)))
; TODO maybe this should power the destruction algorithm also...
(define (grid-runs grid vertical?)
  (define (ij->loc [i : Integer] [j : Integer])
    (if vertical?
        (make-loc i j)
        (make-loc j i)))
  (let* ([w (grid-width grid)]
         [h (grid-height grid)]
         [([imax : Integer] [jmax : Integer])
          (if vertical?
              (values w h)
              (values h w))]
         [runs : (Listof Run) '()])
    (for ([i (in-range imax)])
      (let ([current-run : Run '()])
        (define (maybe-finish-run! [color : (U #f Color)])
          (when (and (not (empty? current-run))
                     (not (equal? color (occupant-color (cdr (car current-run))))))
            (set! runs (cons current-run runs))
            (set! current-run '())))
        (for ([j (in-range jmax)])
          (let* ([loc (ij->loc i j)]
                 [occ (grid-get grid loc)]
                 [color (and occ (occupant-color occ))])
            (maybe-finish-run! color)
            (when occ
              (set! current-run (cons (cons loc occ) current-run)))))
        (maybe-finish-run! #f)))
    runs))

(: grid-problems (-> Grid (Listof Run)))
; A run is problematic if it contains fuel or is above any other fuel.
(define (grid-problems grid)

  (define (get-y [x : (Pairof Loc Occupant)])
    (loc-y (car x)))

  (: fuel-below? (-> Integer Integer Boolean))
  (define (fuel-below? [x : Integer] [y : Integer])
    (if (y . < . 0)
        #f
        (or (fuel? (grid-get grid (make-loc x y)))
            (fuel-below? x (sub1 y)))))

  (define (problematic? [run : Run])
    (or (ormap (lambda ([item : (Pairof Loc Occupant)])
                 (fuel? (cdr item)))
               run)
        (let* ([ys (map get-y run)]
               [y (apply min ys)]
               [x (loc-x (car (car run)))])
          (fuel-below? x y))))

  (filter problematic? (grid-runs grid #t)))

(: grid-groups (-> Grid (List (Listof Run) (Listof GroupInfo))))
(define (grid-groups grid)
  (define (make [latency : Latency])
    (lambda ([dg : DestructionGroup])
      (group-info dg latency)))

  (let* ([(grid immediate-groups)
          (grid-resolve grid '())]
         ; Do we have any use for <4 groups here?
         [immediate-groups (append immediate-groups
                                   (destroy grid 1))]
         [grid (or (grid-burst grid) grid)]
         [(grid burst-groups)
          (grid-resolve grid '())]
         ; Do we have any use for <4 groups here?
         [burst-groups (append burst-groups
                               (destroy grid 1))]
         ;[grid (grid-shatter grid)]
         ;[(grid shatter-groups)
         ; (grid-resolve grid '())]
         ; Do we have any use for <4 groups here?
         ;[shatter-groups (append shatter-groups
         ;                        (destroy grid 1))]
         ; Maybe better version of shatter - see which column would be most
         ; valuable if it were topped off? And only do that column?
         ; Then repeat somehow?
         [problems (grid-problems grid)]
         [immediate-groups (map (make 'immediate) immediate-groups)]
         [burst-groups (map (make 'burst) burst-groups)]
         #;[shatter-groups (map (make 'shatter) shatter-groups)])
    (list problems
          (append immediate-groups burst-groups #;shatter-groups))))

(: grid-score (-> Grid Integer))
(define (grid-score grid)
  (define width (grid-width grid))
  (define height (grid-height grid))

  ; A vector of mutable scores for each cell.
  ; A negative score is undesirable; a positive score is desirable.
  ; By default, a fuel is worth -100. It becomes more desirable when
  ; it gets put into a larger group.
  (define cell-scores : (Vectorof Fixnum)
    (make-vector (* width height) 0))
  (define (loc->index [loc : Loc])
    (fx+ (loc-x loc) (fx* (loc-y loc) width)))

  ; For now, assume anything that isn't fuel is a catalyst.
  (define catalyst-score -2)

  ; Initialize the cell-scores
  (for ([loc (grid-locs grid)])
    (let* ([occ (grid-get grid loc)]
           [index (loc->index loc)]
           [score (cond
                    [(fuel? occ) -100]
                    [(not occ) 0]
                    [else catalyst-score])])
      (vector-set! cell-scores index score)))

  (: score (-> Destruction GroupInfo Fixnum))
  (define (score item gi)
    (define count (length (destruction-group-items (group-info-dg gi))))
    (case (group-info-latency gi)
      [(immediate)
       (case count
         [(1) -100]
         [(2) -50]
         [(3) -20]
         [else 0])]
      [(burst)
       (case count
         [(1) -100]
         [(2) -40]
         [(3) 10]
         [(4 5) 20]
         [(6 7) 10]
         [else 0])]
      [(shatter)
       (case count
         [(1) -100]
         [(2) -45]
         [(3) 5]
         [(4 5) 10]
         [(6 7) 5]
         [else 0])]))

  (define result (grid-groups grid))
  (define problems (car result))
  (define groups (cadr result))

  (for ([group groups])
    (let* ([dg (group-info-dg group)]
           [items (destruction-group-items dg)])
      (for ([item items])
        (let* ([loc (destruction-loc item)]
               [occ (destruction-occ item)]
               [index (loc->index loc)]
               [score (if (fuel? occ)
                          (score item group)
                          catalyst-score)]
               [current (vector-ref cell-scores index)])
          (when (> score current)
            (vector-set! cell-scores index score))))))

  (define score-a
    (for/fold ([sum : Fixnum 0])
              ([score cell-scores])
      (fx+ sum score)))

  (define score-b
    (for/fold ([sum : Fixnum 0])
              ([run : Run problems])
      (let* ([count (length run)]
             [score (case count
                      [(1) -3000]
                      [(2) -2000]
                      [(3) -1000]
                      [else (fail "a group of 4+ shouldn't be considered a problem at all")])])
        (fx+ sum score))))

  (fx+ score-a score-b))

(define-type MoveSeq (Listof Action))

; A Possibility is a reachable state plus the sequence that reaches it
(define-type Possibility (List State MoveSeq))

(: get-rotations (-> State (Listof Possibility)))
(define (get-rotations state)
  (let* ([mover (state-mover state)]
         #:break (when (not mover)
                   ; no moves are allowed in this state
                   (list))
         [double? (equal? (catalyst-color (mover-occ-a mover))
                          (catalyst-color (mover-occ-b mover)))]
         [seqs : (Listof MoveSeq)
               (if double?
                   (list (list)
                         (list (rotate #t)))
                   (list (list)
                         (list (rotate #t))
                         (list (rotate #t) (rotate #t))
                         (list (rotate #f))))])
    (for/list ([seq seqs])
      (let* ([result (state-apply state seq)]
             [new-state (car result)]
             #:break (when (not new-state)
                       (fail "rotation failed unexpectedly"))
             [poss : Possibility (list new-state seq)])
        poss))))

(: try (-> Possibility (U Action (Listof Action)) (U #f Possibility)))
(define (try poss action)
  (let* ([old-state : State (car poss)]
         [old-seq : MoveSeq (cadr poss)]
         [new-seq : MoveSeq
                  (if (list? action)
                      action
                      (list action))]
         [result (state-apply old-state new-seq)]
         [new-state (car result)])
    (and new-state
         (list new-state (append old-seq new-seq)))))

(: foo (-> Action Boolean (-> Possibility (Listof Possibility))))
(define (foo action repeat?)
  (: go (-> Possibility (Listof Possibility)))
  (define (go [poss : Possibility])
    (let* ([result (try poss action)])
      (if result
          (if repeat?
              (cons result (go result))
              (list result))
          (list))))
  go)

(: plummet-or-burst (-> Possibility (Listof Possibility)))
; It seems like it would be more natural to return both possibilities and
; decide later whether to plummet or to burst.
; But for now, making that decision here is easier.
(define (plummet-or-burst poss)
  (let* ([plummet-p (try poss (plummet))]
         #:break (when (not plummet-p)
                   ; if plummet fails, then burst will fail also
                   (list))
         [burst-p (try poss (list (plummet) (burst)))]
         #:break (when (not burst-p)
                   ; burst unavailable, just plummet
                   (list plummet-p))
         [(burst-grid burst-groups)
          (grid-resolve (state-grid (car burst-p)) '())]
         #:break (when ((length burst-groups) . > . 3)
                   ; For now, just always burst when we get >3 groups
                   (list burst-p))
         #:break (when (= 0 (grid-count burst-grid fuel?))
                   ; All fuel cleared
                   (list burst-p)))
    (list plummet-p)))

(: get-possibilities (-> State (Listof Possibility)))
(define (get-possibilities state)
  (let* ([all (get-rotations state)]
         [all (let* ([left (flatmap (foo (move 'l) #t) all)]
                     [right (flatmap (foo (move 'r) #t) all)])
                (append all left right))]
         [all (flatmap plummet-or-burst all)])
    all))

(: choose-move (-> State (U #f Possibility)))
; Chooses the AI's favorite move and returns the resulting state
; plus the list of actions needed to reach that state.
(define (choose-move state)
  (define (score [p : Possibility])
    (grid-score (state-grid (car p))))
  (let ([possibilities (get-possibilities state)])
    (and (pair? possibilities)
         (argmax score possibilities))))

(module+ test
  (require (except-in typed/rackunit fail))

  (define-syntax-rule (make-state pattern #:queue queue)
    (parse-state 'pattern 'queue))

  (let* ([grid (parse-grid '([.. y^ ..]
                             [.. y_ ..]
                             [b^ RR ..]
                             [b_ .. ..]))]
         [runs (grid-runs grid #t)]
         [problems (grid-problems grid)])
    (check-equal? 3 (length runs))
    (check-equal? 2 (length problems)))

  ; fast-forward until the state accepts input
  (: accept-input (-> State State))
  (define (accept-input state)
    (if (state-mover state)
        state
        (let* ([result (state-apply state (list (tick)))]
               [state (or (car result)
                          (fail "tick failed"))])
          (accept-input state))))

  (: make-moves (-> State (U Zero Positive-Byte) State))
  (define (make-moves state count)
    (if (= 0 count)
        state
        (let* ([state (accept-input state)]
               [choice (or (choose-move state)
                           (fail "could not choose move"))]
               [seq (cadr choice)]
               [result (state-apply state seq)]
               [new-state (car result)])
          (if new-state
              (make-moves new-state (sub1 count))
              (fail "failed to apply plan")))))

  (define-syntax-rule (check-grid state pattern)
    ; Even a naked check-equal? does not preserve srclocs propertly,
    ; so don't worry about trying to improve this
    (check-equal? (print-grid (state-grid state))
                  'pattern))

  (let* ([state (make-state ((.. .. .. .. .. .. .. ..)
                             (.. .. .. .. .. .. .. ..)
                             (.. .. .. .. .. .. .. ..)
                             (.. .. .. .. .. .. .. ..)
                             (.. .. .. .. .. .. .. ..)
                             (.. .. .. .. .. .. .. ..)
                             (.. .. .. .. .. .. .. ..)
                             (.. .. .. .. .. .. .. ..)
                             (.. .. .. .. .. .. .. ..)
                             (.. RR BB .. .. .. .. ..)
                             (RR .. .. .. BB .. .. YY)
                             (BB .. BB .. .. .. RR ..)
                             (YY .. .. YY .. .. .. RR)
                             (.. .. .. .. YY .. .. ..)
                             (-- -- -- -- -- -- -- --))
                            #:queue [<r y> <b r>])]
         [state (make-moves state 2)])
    (check-grid state ((.. .. .. .. .. .. .. ..)
                       (.. .. .. .. .. .. .. ..)
                       (.. .. .. .. .. .. .. ..)
                       (.. .. .. .. .. .. .. ..)
                       (.. .. .. .. .. .. .. ..)
                       (.. .. .. .. .. .. .. ..)
                       (.. .. .. .. .. .. .. ..)
                       (.. .. .. .. .. .. .. ..)
                       (.. <r b> .. .. .. .. ..)
                       (.. RR BB .. .. .. <r y>)
                       (RR .. .. .. BB .. .. YY)
                       (BB .. BB .. .. .. RR ..)
                       (YY .. .. YY .. .. .. RR)
                       (.. .. .. .. YY .. .. ..)
                       (-- -- -- -- -- -- -- --))))

  (let* ([state (make-state ((.. .. .. .. .. .. .. ..)
                             (.. .. .. .. .. .. .. ..)
                             (.. .. .. .. .. .. .. ..)
                             (.. .. .. .. .. .. .. ..)
                             (.. .. .. .. .. .. .. ..)
                             (.. .. .. .. .. .. .. ..)
                             (.. .. .. .. .. .. .. ..)
                             (.. .. .. .. .. .. .. ..)
                             (.. .. .. .. .. .. .. ..)
                             (.. BB .. .. YY .. .. ..)
                             (YY .. .. .. YY RR .. BB)
                             (.. .. .. RR BB RR .. ..)
                             (.. .. .. .. .. YY .. ..)
                             (.. BB .. .. .. .. BB ..)
                             (-- -- -- -- -- -- -- --))
                            #:queue [<y y> <r o> <r y>])]
         [state (make-moves state 3)])
    (check-grid state ((.. .. .. .. .. .. .. ..)
                       (.. .. .. .. .. .. .. ..)
                       (.. .. .. .. .. .. .. ..)
                       (.. .. .. .. .. .. .. ..)
                       (.. .. .. .. .. .. .. ..)
                       (.. .. .. .. .. .. .. ..)
                       (.. .. .. .. .. .. .. ..)
                       (.. .. .. .. <y r> .. ..)
                       (y^ .. .. .. <o r> .. ..)
                       (y_ BB .. .. YY .. .. ..)
                       (YY .. .. .. YY RR .. BB)
                       (.. .. .. RR BB RR .. ..)
                       (.. .. .. .. .. YY .. ..)
                       (.. BB .. .. .. .. BB ..)
                       (-- -- -- -- -- -- -- --))))
  )
