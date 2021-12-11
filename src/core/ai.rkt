#lang typed/racket

(provide choose-move)

(require racket/fixnum
         "../typed-utils.rkt"
         "data.rkt"
         "grid.rkt"
         "state.rkt")

(module+ test
  (define (show x)
    (let* ([(item width)
            (if (grid? x)
                (values (print-grid x)
                        (+ 5 (* 3 (grid-width x))))
                (values x (pretty-print-columns)))])
      (parameterize ([pretty-print-columns width])
        (pretty-print item))))

  (define-syntax-rule (test-helper condition id ...)
    (when (not condition)
      (displayln "\n\n === Test Failure ===")
      (begin
        (displayln (format "value of ~a is" 'id))
        (show id))
      ...
      (fail "Condition failed" 'condition)))

  (define-syntax (check stx)
    (syntax-case stx (>)
      [(_ a > b)
       (syntax/loc stx
         (test-helper ((grid-score a) . > . (grid-score b))
                      a b))]))

  (define-syntax-rule (make-state pattern #:queue queue)
    (parse-state 'pattern 'queue)))

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

(define-type Run (Listof (Pairof Loc Occupant)))

(: run-color (-> Run Color))
(define (run-color run)
  (or (occupant-color (cdr (car run)))
      (fail "illegal run was constructed - lacks color")))

(: has-fuel? (-> Run Boolean))
(define (has-fuel? run)
  (match run
    [(list) #f]
    [(list (cons loc occ) more ...)
     (or (fuel? occ)
         (has-fuel? more))]))

(: problematic? (-> Run Grid Boolean))
; A run is problematic if it blocks vertical access to fuel.
(define (problematic? run grid)

  (define (get-y [x : (Pairof Loc Occupant)])
    (loc-y (car x)))

  (: fuel-below? (-> Integer Integer Boolean))
  (define (fuel-below? [x : Integer] [y : Integer])
    (if (y . < . 0)
        #f
        (or (fuel? (grid-get grid (make-loc x y)))
            (fuel-below? x (sub1 y)))))

  (let* ([ys (map get-y run)]
         [y (sub1 (apply min ys))]
         [x (loc-x (car (car run)))])
    (fuel-below? x y)))

(: fxsum (-> (Listof Fixnum) Fixnum))
(define (fxsum nums)
  (for/fold ([sum : Fixnum 0])
            ([num nums])
    (fx+ sum num)))

(struct info ([occ : Occupant]
              [vertical-run : (Boxof Run)]
              ; A broken run is a run that "sees through" blank catatlysts and empty space.
              ; TODO have to solve the fuel blockage problem though!
              [vertical-broken-run : (Boxof Run)])
  #:transparent)

(: analyze (-> Grid (List (Vectorof (Vectorof (U #f info)))
                          (Listof Run) ; vertical-runs
                          (Listof Run) ; vertical-broken-runs
                          )))
(define (analyze [grid : Grid])
  (define width (grid-width grid))
  (define height (grid-height grid))

  (define vertical-runs : (Listof Run) '())
  (define vertical-broken-runs : (Listof Run) '())

  (define-type Column (Vectorof (U #f info)))
  (define (empty-column)
    (ann (make-vector height #f) Column))

  (define columns : (Vectorof Column)
    (make-vector width (empty-column)))

  (for ([x (in-range (grid-width grid))])
    (let ([column : Column (empty-column)]
          [run-box : (U #f (Boxof Run)) #f]
          [broken-run-box : (U #f (Boxof Run)) #f]
          [run-broken? : Boolean #f])
      (vector-set! columns x column)

      ; Determines if the current runs (broken and non-broken) are finished,
      ; and cleans them up. Does not start a new run.
      (define (maybe-finish-runs! [occ : (U #f Occupant)] [done-with-column? : Boolean])
        (let ([color (and occ (occupant-color occ))])
          (let ([rb run-box])
            (when (and rb
                       (not (equal? color (run-color (unbox rb)))))
              (set! vertical-runs (cons (unbox rb) vertical-runs))
              (set! run-box #f)))
          (let* ([brb broken-run-box]
                 #:break (when (not brb) #f)
                 [run (unbox brb)])
            (when (not color)
              (set! run-broken? #t))
            (when (or done-with-column?
                      ; Mismatched color always ends the run:
                      (and color (not (equal? color (run-color run))))
                      ; Fuel ends the run if we have seen a gap:
                      (and run-broken? (fuel? occ)))
              (set! vertical-broken-runs (cons run vertical-broken-runs))
              (set! broken-run-box #f)
              (set! run-broken? #f)))))

      (for ([y (in-range (grid-height grid))])
        (let* ([loc (make-loc x y)]
               [occ (grid-get grid loc)]
               [color (and occ (occupant-color occ))])
          (maybe-finish-runs! occ #f)
          (when (and occ color)
            (let* ([rb : (Boxof Run)
                       (or run-box (box (list)))]
                   [brb : (Boxof Run)
                        (or broken-run-box (box (list)))]
                   [run-item (cons loc occ)])
              (: update-box! (-> (Boxof Run) Any))
              (define (update-box! b)
                (let ([run (unbox b)])
                  (cond
                    [(empty? run)
                     (set-box! b (list run-item))]
                    [(equal? color (run-color run))
                     (set-box! b (cons run-item run))]
                    [else
                     ; If this occupant doesn't match the previous, we should have
                     ; already started a new run
                     (fail "likely bug in maybe-finish-run!" run-item run)])))
              (set! run-box rb)
              (set! broken-run-box brb)
              (update-box! rb)
              (update-box! brb)

              (let ([info (info occ rb brb)])
                (vector-set! column y info))))))

      ; end of column, finish run
      (maybe-finish-runs! #f #t)))

  ; Return value:
  (list columns vertical-runs vertical-broken-runs))

(: peak-broken? (-> Grid Integer (Vectorof (U #f info)) Boolean))
(define (peak-broken? grid x column)
  (let loop ([y (sub1 (vector-length column))])
    (let* (#:break (when (y . < . 0)
                     #f)
           [blank? (let ([occ (grid-get grid (make-loc x y))])
                     (and (catalyst? occ)
                          (not (catalyst-color occ))))]
           #:break (when blank? #t)
           [info (vector-ref column y)]
           #:break (when (not info)
                     (loop (sub1 y)))
           [occ (info-occ info)]
           [vr (unbox (info-vertical-run info))]
           [vbr (unbox (info-vertical-broken-run info))])
      (cond
        ; TODO blank catatlysts are not recorded in the column. That is why
        ; we need the grid and the x-coordinate.
        ; Being able to uncomment this would feel nicer:
        #;[(and (catalyst? occ)
                (not (catalyst-color occ)))
           #t]
        [(and ((length vr) . < . 4)
              ((length vbr) . > . (length vr)))
         #t]
        [else #f]))))

(define debug-score? : (Parameterof Boolean) (make-parameter #f))

(: grid-score (-> Grid Fixnum))
(define (grid-score grid)
  (let* ([width (grid-width grid)]
         [(grid-a _)
          (grid-resolve grid '())]
         [res-a (analyze grid-a)]
         [cols-a (first res-a)]
         [runs-a (second res-a)]
         [broken-runs-a (third res-a)]
         [grid-b (or (grid-burst grid) grid)]
         [(grid-b _)
          (grid-resolve grid-b '())]
         [res-b (analyze grid-b)]
         [cols-b (first res-b)]
         [runs-b (second res-b)]
         [broken-runs-b (third res-b)]
         [burst-score
          (fxsum
           (for/list ([loc (grid-locs grid)])
             (let* ([col-a (vector-ref cols-a (loc-x loc))]
                    [info-a (vector-ref col-a (loc-y loc))]
                    [occ-a : (U #f Occupant)
                           (and info-a (info-occ info-a))]
                    [col-b (vector-ref cols-b (loc-x loc))]
                    [info-b (vector-ref col-b (loc-y loc))]
                    [occ-b : (U #f Occupant)
                           (and info-b (info-occ info-b))])
               (cond
                 [(and (fuel? occ-a)
                       (not (fuel? occ-b)))
                  100]
                 [(and (fuel? occ-a)
                       (fuel? occ-b))
                  (let* ([count-a (length (unbox (info-vertical-run (or info-a (fail "TODO")))))]
                         [count-b (length (unbox (info-vertical-run (or info-b (fail "TODO")))))]
                         [diff (- count-b count-a)])
                    (case count-b
                      [(1) 0]
                      [(2) (fx* 10 diff)]
                      [(3) (fx* 20 diff)]
                      [else (fail "impossible, right?")]))]
                 [else 0]))))]
         [run-score
          (fxsum
           (for/list ([run : Run runs-b])
             (let* ([occs : (Listof Occupant) (map (lambda ([x : (Pairof Loc Occupant)])
                                                     (cdr x)) run)]
                    [has-fuel? (ormap fuel? occs)]
                    [count (length run)])
               (case count
                 [(1) -10]
                 [(2) -7]
                 [(3) -2]
                 [else 0]))))]
         [problem-score
          (fxsum
           (for/list ([run : Run broken-runs-b])
             (if (problematic? run grid-b)
                 (case (length run)
                   [(1) -30000]
                   [(2) -20000]
                   [(3) -10000]
                   [else 0])
                 0)))]
         [fuel-score
          (fxsum (for/list ([run runs-b])
                   (if (has-fuel? run)
                       (case (length run)
                         [(1) -3000]
                         [(2) -2000]
                         [(3) -1000]
                         [else 0])
                       0)))]
         [broken-peak-score
          (fxsum
           (for/list ([x (in-range width)])
             (let ([col (vector-ref cols-a x)])
               (if (peak-broken? grid-a x col) 50 0))))]
         [score (fxsum (list burst-score run-score problem-score
                             fuel-score broken-peak-score))])
    (when (debug-score?)
      (println (list "burst:" burst-score
                     "run:" run-score
                     "problem:" problem-score
                     "fuel:" fuel-score
                     "broken peak:" broken-peak-score
                     "total:" score)))
    score))

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
; Some hard-coded decisions of when to burst
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
         [burst-count (length burst-groups)]
         #:break (when ((length burst-groups) . > . 3)
                   ; For now, just always burst when we get >3 groups
                   (list burst-p))
         [(plummet-grid plummet-groups)
          (grid-resolve (state-grid (car plummet-p)) '())]
         [plummet-count (length plummet-groups)]
         #:break (when (and (plummet-count . > . 0)
                            (burst-count . > . plummet-count))
                   ; Not sure about this, but seems to make sense
                   (list burst-p))
         #:break (when (= 0 (grid-count burst-grid fuel?))
                   ; All fuel cleared
                   (list burst-p)))
    (list plummet-p burst-p)))

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
  (let* ([possibilities (get-possibilities state)]
         [result (and (pair? possibilities)
                      (argmax score possibilities))])
    #;(when result
        (pretty-print (print-grid (state-grid state)))
        (pretty-print (print-grid (state-grid (car result)))))
    result))

(module+ test
  (debug-score? #t)

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

  (let* ([g1 (parse-grid '((.. .. .. .. .. .. .. ..)
                           (.. .. .. .. .. .. .. ..)
                           (.. .. .. .. .. .. .. ..)
                           (.. .. .. .. .. .. .. ..)
                           (.. .. .. .. .. .. .. ..)
                           (.. .. .. .. .. .. .. ..)
                           (.. .. .. .. .. .. .. ..)
                           (.. .. .. .. .. .. .. ..)
                           (.. .. .. .. .. .. .. ..)
                           (.. RR BB .. .. .. <r y>)
                           (RR .. .. .. BB .. .. YY)
                           (BB .. BB .. .. .. RR ..)
                           (YY .. .. YY .. .. .. RR)
                           (.. .. .. .. YY .. .. ..)
                           (-- -- -- -- -- -- -- --)))]
         [g2 (parse-grid '((.. .. .. .. .. .. .. ..)
                           (.. .. .. .. .. .. .. ..)
                           (.. .. .. .. .. .. .. ..)
                           (.. .. .. .. .. .. .. ..)
                           (.. .. .. .. .. .. .. ..)
                           (.. .. .. .. .. .. .. ..)
                           (.. .. .. .. .. .. .. ..)
                           (.. .. .. .. .. .. .. ..)
                           (.. .. .. .. .. .. .. ..)
                           (.. RR BB .. .. .. .. ..)
                           (RR .. .. r^ BB .. .. YY)
                           (BB .. BB y_ .. .. RR ..)
                           (YY .. .. YY .. .. .. RR)
                           (.. .. .. .. YY .. .. ..)
                           (-- -- -- -- -- -- -- --)))])
    (check g1 > g2))

  (let ([g1 (parse-grid '((.. .. .. .. .. .. .. ..)
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
                          (-- -- -- -- -- -- -- --)))]
        ; Above, the <r hanging over the RR is not a problem.
        [g2 (parse-grid '((.. .. .. .. .. .. .. ..)
                          (.. .. .. .. .. .. .. ..)
                          (.. .. .. .. .. .. .. ..)
                          (.. .. .. .. .. .. .. ..)
                          (.. .. .. .. .. .. .. ..)
                          (.. .. .. .. .. .. .. ..)
                          (.. .. .. .. .. .. .. ..)
                          (.. .. .. .. .. .. .. ..)
                          (.. <r b> .. .. .. .. ..)
                          (.. RR BB .. .. .. .. ..)
                          (RR .. .. .. BB <y r> YY)
                          (BB .. BB .. .. .. RR ..)
                          (YY .. .. YY .. .. .. RR)
                          (.. .. .. .. YY .. .. ..)
                          (-- -- -- -- -- -- -- --))
                        )])
    (check g1 > g2))

  (let ([g1 (parse-grid '((.. .. .. .. .. .. .. ..)
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
                          (-- -- -- -- -- -- -- --)))]
        [g2 (parse-grid '((.. .. .. .. .. .. .. ..)
                          (.. .. .. .. .. .. .. ..)
                          (.. .. .. .. .. .. .. ..)
                          (.. .. .. .. .. .. .. ..)
                          (.. .. .. .. .. .. .. ..)
                          (.. .. .. .. .. .. .. ..)
                          (.. .. .. .. .. .. .. ..)
                          (.. .. .. .. .. .. .. ..)
                          (.. .. .. .. .. .. .. ..)
                          (.. RR BB .. <b r> <r y>)
                          (RR .. .. .. BB .. .. YY)
                          (BB .. BB .. .. .. RR ..)
                          (YY .. .. YY .. .. .. RR)
                          (.. .. .. .. YY .. .. ..)
                          (-- -- -- -- -- -- -- --)))])
    (check g1 > g2))

  (let* ([g1 (parse-grid '((.. .. .. .. .. .. .. ..)
                           (.. .. .. .. .. .. .. ..)
                           (.. .. .. .. .. .. .. ..)
                           (.. .. .. .. .. .. .. ..)
                           (.. .. .. .. .. .. .. ..)
                           (.. .. .. .. .. .. .. ..)
                           (.. .. .. .. .. .. .. ..)
                           (.. .. .. .. .. .. .. ..)
                           (y^ .. .. .. .. .. .. ..)
                           (y_ BB .. .. YY .. .. ..)
                           (YY .. .. .. YY RR .. BB)
                           (.. .. .. RR .. RR .. ..)
                           (.. .. .. .. .. YY .. ..)
                           (.. BB .. .. .. .. BB ..)
                           (-- -- -- -- -- -- -- --)))]
         ; We favor going from 1 to 3 over going from 2 to 4 (non-latent of course)
         [g2 (parse-grid '((.. .. .. .. .. .. .. ..)
                           (.. .. .. .. .. .. .. ..)
                           (.. .. .. .. .. .. .. ..)
                           (.. .. .. .. .. .. .. ..)
                           (.. .. .. .. .. .. .. ..)
                           (.. .. .. .. .. .. .. ..)
                           (.. .. .. .. .. .. .. ..)
                           (.. .. .. .. y^ .. .. ..)
                           (.. .. .. .. y_ .. .. ..)
                           (.. BB .. .. YY .. .. ..)
                           (YY .. .. .. YY RR .. BB)
                           (.. .. .. RR .. RR .. ..)
                           (.. .. .. .. .. YY .. ..)
                           (.. BB .. .. .. .. BB ..)
                           (-- -- -- -- -- -- -- --)))])
    (check g1 > g2))

  (let* ([g1 (parse-grid '((.. .. .. .. .. .. .. ..)
                           (.. .. .. .. .. .. .. ..)
                           (.. .. .. .. .. .. .. ..)
                           (.. .. .. .. .. .. .. ..)
                           (.. .. .. .. .. .. .. ..)
                           (.. .. .. .. .. .. .. ..)
                           (.. .. .. .. .. .. .. ..)
                           (.. .. .. .. .. .. .. ..)
                           (y^ .. .. .. <o r> .. ..)
                           (y_ BB .. .. YY .. .. ..)
                           (YY .. .. .. YY RR .. BB)
                           (.. .. .. RR BB RR .. ..)
                           (.. .. .. .. .. YY .. ..)
                           (.. BB .. .. .. .. BB ..)
                           (-- -- -- -- -- -- -- --)))]
         [g2 (parse-grid '((.. .. .. .. .. .. .. ..)
                           (.. .. .. .. .. .. .. ..)
                           (.. .. .. .. .. .. .. ..)
                           (.. .. .. .. .. .. .. ..)
                           (.. .. .. .. .. .. .. ..)
                           (.. .. .. .. .. .. .. ..)
                           (.. .. .. .. .. .. .. ..)
                           (.. .. .. .. .. .. .. ..)
                           (y^ .. .. .. .. .. .. ..)
                           (y_ BB .. .. YY <r o> ..)
                           (YY .. .. .. YY RR .. BB)
                           (.. .. .. RR BB RR .. ..)
                           (.. .. .. .. .. YY .. ..)
                           (.. BB .. .. .. .. BB ..)
                           (-- -- -- -- -- -- -- --)))])
    (check g1 > g2))

  (let ([g1 (parse-grid '([<r y> .. ..]
                          [r^ .. .. ..]
                          [o_ .. .. o^]
                          [rr .. .. y_]
                          [RR .. .. YY]))]
        [g2 (parse-grid '([.. .. .. ..]
                          [r^ .. <r y>]
                          [o_ .. .. o^]
                          [rr .. .. y_]
                          [RR .. .. YY]))])
    (check g1 > g2))

  (let ([g1 (parse-grid '((.. .. .. .. .. .. .. ..)
                          (.. .. .. .. .. .. .. ..)
                          (.. .. .. .. .. .. .. ..)
                          (.. .. .. .. .. .. .. ..)
                          (.. .. .. .. .. .. .. ..)
                          (.. .. b^ .. .. .. .. ..)
                          (.. .. o_ .. .. .. .. ..)
                          (.. <y b> .. .. .. .. ..)
                          (.. <y b> .. .. .. .. yy)
                          (.. YY BB .. .. .. .. yy)
                          (.. .. .. .. .. .. .. YY)
                          (.. .. .. .. .. .. .. ..)
                          (.. YY BB .. .. .. .. ..)
                          (.. .. .. .. .. .. .. ..)
                          (-- -- -- -- -- -- -- --)))]
        [g2 (parse-grid '((.. .. .. .. .. .. .. ..)
                          (.. .. .. .. .. .. .. ..)
                          (.. .. .. .. .. .. .. ..)
                          (.. .. .. .. .. .. .. ..)
                          (.. .. .. .. .. .. .. ..)
                          (.. .. .. .. .. .. .. ..)
                          (.. .. .. .. .. .. .. ..)
                          (.. <y b> .. .. .. .. ..)
                          (.. <y b> .. .. .. .. yy)
                          (.. YY BB .. .. .. .. yy)
                          (.. .. .. .. .. .. .. YY)
                          (.. .. .. .. .. .. .. ..)
                          (.. YY BB .. .. .. .. ..)
                          (.. .. .. <b o> .. .. ..)
                          (-- -- -- -- -- -- -- --)))])
    (check g1 > g2))

  ; The sideways blank in g1 is preferred over the vertical blank in g2 and g3
  ; because both runs are now broken instead of just the red one.
  (let ([g1 (parse-grid '([.. .. ..]
                          [.. <o r>]
                          [.. YY ..]
                          [.. YY RR]
                          [.. YY RR]))]
        [g2 (parse-grid '([.. .. ..]
                          [.. .. o^]
                          [.. YY r_]
                          [.. YY RR]
                          [.. YY RR]))]
        [g3 (parse-grid '([.. .. ..]
                          [.. .. r^]
                          [.. YY o_]
                          [.. YY RR]
                          [.. YY RR]))])
    (check g1 > g2)
    (check g1 > g3))

  (let ([g1 (parse-grid '([.. .. .. .. ..]
                          [.. .. .. .. ..]
                          [.. .. .. .. ..]
                          [.. .. .. .. ..]
                          [.. r^ .. .. ..]
                          [yy o_ bb .. ..]
                          [<y r> bb .. RR]
                          [.. <r b> .. RR]
                          [.. RR .. .. RR]
                          [.. YY .. .. ..]
                          [yy .. BB .. ..]
                          [YY .. .. .. ..]))]
        [g2 (parse-grid '([.. .. .. .. ..]
                          [.. .. .. .. ..]
                          [.. .. .. .. ..]
                          [.. .. .. .. ..]
                          [.. .. .. .. r^]
                          [yy .. bb .. o_]
                          [<y r> bb .. RR]
                          [.. <r b> .. RR]
                          [.. RR .. .. RR]
                          [.. YY .. .. ..]
                          [yy .. BB .. ..]
                          [YY .. .. .. ..]))])
    (check g1 > g2))

  (let ([g1 (parse-grid '([.. .. .. ..]
                          [.. .. .. ..]
                          [.. .. .. ..]
                          [.. .. .. ..]
                          [.. .. .. ..]
                          [.. .. .. b^]
                          [.. .. .. o_]
                          [.. .. .. BB]
                          [.. .. .. BB]
                          [.. .. .. BB]
                          [.. .. .. ..]
                          [.. .. YY RR]))]
        ; g2 demonstrates a special kind of problem, where the upper blues
        ; are blocking access to the lower blue because they are the same color.
        ; In g1 the lower color is red, so it is not a problem; you are able
        ; to make progress on the red without bursting the blues.
        [g2 (parse-grid '([.. .. .. ..]
                          [.. .. .. ..]
                          [.. .. .. ..]
                          [.. .. .. ..]
                          [.. .. .. ..]
                          [.. .. .. b^]
                          [.. .. .. o_]
                          [.. .. .. BB]
                          [.. .. .. BB]
                          [.. .. .. BB]
                          [.. .. .. ..]
                          [.. .. YY BB]))])
    (check g1 > g2))

  (let ([g1 (parse-grid '([.. .. .. ..]
                          [.. .. .. ..]
                          [.. .. .. ..]
                          [.. .. .. ..]
                          [.. .. .. ..]
                          [.. .. .. ..]
                          [r^ .. .. ..]
                          [r_ rr .. RR]))]
        ; g1 bursted, g2 did not.
        ; We don't want to burst prematurely, but in this case we need to recognize
        ; that not bursting is causing us to be unable to do anything useful.
        ; THINK should this be handled by grid-score, or would simply going to a 2-ply
        ; analysis fix it? It wouldn't even have to be a full 2-ply, it could be
        ; that after we pick our favorite move we back up and say, "okay, let's
        ; consider burst vs plummet and see which is better after one more ply."
        ; OR maybe my recent scoring adjustment has naturally fixed this?
        [g2 (parse-grid '([.. .. <y b>]
                          [.. <r y> ..]
                          [.. .. <y o>]
                          [.. .. .. bb]
                          [.. .. .. bb]
                          [.. .. YY BB]
                          [r^ .. .. ..]
                          [r_ .. .. RR]))])
    (check g1 > g2))

  (let ([g1 (parse-grid '([<y b> .. ..]
                          [<y b> .. ..]
                          [.. bb .. ..]
                          [<y b> .. ..]
                          [YY .. .. ..]
                          [.. BB .. ..]))]
        ; The b's in g2 should not form a broken run with the BB below.
        ; Obviously it is a dependency problem, but what is the general solution?
        ;
        ; Let's look at g2 and say that
        ;    run-y1 is the top left <y
        ;    run-b3 is the run of 3 b
        ;    run-y2 is the two yellows below run-y1
        ; Now we can observe that
        ; 1) run-y1 cannot fall unless run-b3 is eliminated
        ; 2) run-b3 cannot fall unless run-y2 is eliminated
        ; 3) run-y2 is blocked by run-y1
        ; Therefore making both claims
        ;   - run-y1 forms a broken run with run-y2
        ;   - run-b3 forms a broken run with the BB below it
        ; is not correct. At most one of those can be true.
        ; To get started, let's just detect this situation and
        ; say that neither of them are broken runs and see how the AI plays.
        ; That should be good enough to make it so that it won't create this
        ; situation in the first place.
        ; Or what if we just harshly penalize any detected circular dependency?
        ; That should motivate the AI to clear it ASAP.
        ; (Unrelated, we should prioritize problems at higher y-coordinates.)
        [g2 (parse-grid '([.. .. .. ..]
                          [<y b> .. ..]
                          [.. bb .. ..]
                          [<y b> .. ..]
                          [YY .. .. ..]
                          [.. BB <b y>]))])
    (check g1 > g2))
  )
