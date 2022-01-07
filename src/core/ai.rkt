#lang typed/racket

(provide choose-move)

(require racket/fixnum
         "../typed-utils.rkt"
         "cell-names.rkt"
         "data.rkt"
         "grid.rkt"
         "state.rkt")

(module+ test
  (require (for-syntax syntax/parse))

  ; Set to #t to help debug grid scoring
  (explain-score? #f)

  ; Some test helpers that need to be defined before we can get going:
  (define (show x)
    (define (width-for-grid [g : Grid])
      (+ 8 (* 3 (grid-width g))))
    (let* ([(item width)
            (cond
              [(grid? x)
               (values `(parse-grid ',(print-grid x))
                       (width-for-grid x))]
              [(analysis? x)
               (let ([grid (analysis-grid x)])
                 (values `(grid-analysis (parse-grid ',(print-grid grid)))
                         (+ 4 (width-for-grid grid))))]
              [else
               (values x #f)])])
      (if width
          (parameterize ([pretty-print-columns width])
            (pretty-display item))
          (pretty-print item))))

  (define-syntax-rule (grid-analyze [row-num spec ...] ...)
    ; discard the row-num
    (let* ([symgrid '([spec ...]
                      ...)]
           ; drop the last row which contains column names
           [symgrid (cdr (reverse symgrid))]
           [symgrid (reverse symgrid)]
           [grid (parse-grid symgrid)])
      (grid-analysis grid))))



; == Definition of "Stable" ==
; An occupant is stable if fuel in its own column is preventing it from dropping.
; A more precise definition of stability:
; * Fuel is stable
; * TODO should we add "ground is stable" here?
; * A non-blank occupant above a stable occupant is stable
; When an occupant is stable, we don't expect it to fall any further
; than it already has. (Horizontal destruction could cause it to fall,
; but we usually don't care about this possibility.)
(module+ test
  (let ([a (grid-analyze
            [-7 <y r> .. bb]
            [-6 oo .. rr ..]
            [-5 yy .. <b y>]
            [-4 <r y> .. rr]
            [-3 bb .. .. RR]
            [-2 BB .. .. ..]
            [-1 .. .. .. ..]
            [-- A: B: C: D:])])
    ; These occupants are supported by A2
    (check #:stable? a A2 A3 A4 A5)
    ; A6 is blank so it is not stable
    (check #:unstable? a A6 A7)
    ; Columns B and C lack fuel
    (check #:unstable? a B4 B7 C5 C6)
    ; These occupants are supported by D3
    (check #:stable? a D3 D4 D5)
    ; D7 is definitely not stable, it's falling right now
    (check #:unstable? a D7)))

; == Definition of "Pillar" ==
; A pillar is a vertical sequence of one or more consecutive occupants that
; share a single non-blank color.
;
; Assuming* that fuel can never sit on top of a catalyst, a pillar will contain
; either stable or unstable occupants, but never both.
; So we can easily describe a pillar as stable or unstable.
; (* The "dump fuel" penalty could violate this assumption, but let's ignore that.)
(module+ test
  (let ([a (grid-analyze
            [-7 <b r> .. bb]
            [-6 bb .. yy ..]
            [-5 oo rr bb ..]
            [-4 <b r> <b b>]
            [-3 bb .. .. rr]
            [-2 BB .. .. RR]
            [-1 .. .. .. ..]
            [-- A: B: C: D:])])
    (check #:pillar? a {[A2 A4] ; [lo - hi]
                        [A6 A7]
                        [B4 B5]
                        [B7 B7]
                        [C4 C5]
                        [C6 C6]
                        [D2 D3]
                        [D4 D4]
                        [D7 D7]})))

; == Definition of "Dependency" ==
; A dependency is recorded for each unstable catalyst having a partner to
; its left or to its right.
; An example dependency is [B3 A3] which means "B3 depends on A3" or to be more
; precise "B3 will not drop unless A3 also drops or A3 is destroyed."
; This might be asymmetric: in the following example, [B3 A3] is a dependency
; because B3 is unstable, but [A3 B3] is not a dependency because A3 is stable.
(module+ test
  (let ([a (grid-analyze
            [-6 .. .. ..]
            [-5 <y b> ..]
            [-4 .. bb ..]
            [-3 <y b> ..]
            [-2 YY .. ..]
            [-1 .. BB ..]
            [-- A: B: C:])])
    (check #:deps a {[B3 A3] [B5 A5] [A5 B5]})))

; A dependency is always of the form [Iy Jy] where I and J identify
; two adjacent columns, and y identifies a single y-coordinate.

; == Definition of "Blockage" ==
; Dependencies usually aren't bad.
; Good players regularly use dependencies to create combos.
; But specific dependency patterns can create a "blockage".
; A naive AI (or inexperienced player) might look at the following grid and
; conclude "there are 3 blues above the BB and 2 yellows above the YY; it
; looks like we've nearly prepared a combo."
; But this assessment is incorrect because there is a blockage.
(module+ test
  (let ([a (grid-analyze
            [-6 .. .. ..]
            [-5 <y b> ..]
            [-4 .. bb ..]
            [-3 <y b> ..]
            [-2 YY .. ..]
            [-1 .. BB ..]
            [-- A: B: C:])])
    (check #:deps a {[B3 A3] [B5 A5] [A5 B5]})
    (check #:blockages a {([B5 A5] [B3 A3])})))
; We have a single blockage, namely ([B5 A5] [B3 A3]).
; This can be read as "the [B5 A5] dependency blocks the [B3 A3] dependency".
; In general a blockage is a pair of dependencies ([Pi Qi] [Pj Qj]) such that
; * i > j
; * Qi and Qj do not share a pillar
; * Every cell between Pi and Pj is occupied by a non-blank
;   TODO need to try this 3rd bullet point out...
;   It seems like it could handle the pathological case better.
;   Or it could handle it worse!
;   We don't want to get in a situation where blockage B only gets noticed
;   after clearing blockage A, because then the AI won't see much value
;   in clearing blockage A.

; In the previous example, A3 and A5 do not share a pillar because A4 is an
; empty cell. But a color-mismatch could also cause a blockage.
; Or even if the A4 cell was a blank catalyst it would still be a blockage.


(module+ test
  (let ([a (grid-analyze
            [-5 .. .. ..]
            [-4 <y r> ..]
            [-3 <y r> ..]
            [-2 YY .. ..]
            [-1 .. RR ..]
            [-- A: B: C:])])
    (check #:deps a {[B3 A3] [B4 A4]})
    (check #:blockages a {}))
  ; Above lacks the blockage ([B4 A4] [B3 A3]) because A4 and A3 share a pillar.
  ; Changing the A4 cell can cause a blockage, as follows
  (let ([a (grid-analyze
            [-5 .. .. ..]
            [-4 <b r> ..]
            [-3 <y r> ..]
            [-2 YY .. ..]
            [-1 .. RR ..]
            [-- A: B: C:])])
    (check #:deps a {[B3 A3] [B4 A4]})
    (check #:blockages a {([B4 A4] [B3 A3])}))
  ; Now the blockage does exist because A4 and A3 no longer share a pillar due to
  ; the color mismatch.
  )

; A new example with no blockages:
(module+ test
  (let ([a (grid-analyze
            [-5 .. .. ..]
            [-4 <y r> ..]
            [-3 .. <r b>]
            [-2 .. .. BB]
            [-1 YY RR ..]
            [-- A: B: C:])])
    (check #:deps a {[B3 C3] [B4 A4] [A4 B4]})
    (check #:blockages a {})))

; Okay, how about this one:
(module+ test
  #;(grid-analyze
     [-6 .. <r b>]
     [-5 .. .. bb]
     [-4 <y r> oo]
     [-3 .. <r b>]
     [-2 .. .. BB]
     [-1 YY RR ..]
     [-- A: B: C:])
  ; Our algorithm will definitely not like this as-is, so let's burst and
  ; fall before we analyze. Then we get:
  (let ([a (grid-analyze
            [-6 .. .. ..]
            [-5 .. <r b>]
            [-4 <y r> bb]
            [-3 .. <r b>]
            [-2 .. .. BB]
            [-1 YY RR ..]
            [-- A: B: C:])])
    (check #:deps a {[B3 C3] [B4 A4] [A4 B4] [B5 C5]})
    (check #:blockages a {}))
  ; Again we have the situation where ([B5 C5] [B3 C3]) is not a blockage
  ; because C3 and C5 share a pillar.
  ; The moral of this story is that we need to burst and drop (and maybe even
  ; run the destroy<->fall cycle) before doing blockage analysis.
  )

; How about a pathological case:
(module+ test
  (let ([a (grid-analyze
            [10 <y b> .. .. ..]
            [-9 .. <b r> .. ..]
            [-8 .. .. <r y> ..]
            [-7 .. .. .. <y b>]
            [-6 .. .. .. .. bb]
            [-5 .. .. .. <y b>]
            [-4 .. .. <r y> ..]
            [-3 .. <b r> .. ..]
            [-2 <y b> .. .. ..]
            [-1 YY BB RR YY BB]
            [-- A: B: C: D: E:])])
    (check #:deps a {[A10 B10]
                     [B10 A10] [B9 C9]
                     [C3 B3] [C4 D4] [C8 D8] [C9 B9]
                     [D4 C4] [D5 E5] [D7 E7] [D8 C8]
                     [E5 D5] [E7 D7]})
    (check #:blockages a
           {([E7 D7] [E5 D5])
            ;([D7 E7] [D5 E5]) nope, because E5 and E7 do share a pillar
            ([D8 C8] [D4 C4])
            ([C8 D8] [C4 D4])
            ([C9 B9] [C3 B3])})))
; That seems reasonable.
; If we look at the first cell named by each blockage, we see E7 D8 C8 and C9
; and those would certainly be nice to clear.
; And that principle seems to hold with all the previous examples.
; Can I find a counterexample to the "first cell named" idea?
; How about this:
(module+ test
  (let ([a (grid-analyze
            [-5 <b r>]
            [-4 .. yy]
            [-3 <b r>]
            [-2 BB RR]
            [-1 .. ..]
            [-- A: B:])])
    (check #:deps a {[A5 B5]})
    (check #:blockages a {})))
; This is actually OK.
; There's nothing tricky going on; a simple algorithm should not get
; confused into thinking that B5 might join with B2 and B3 in the future.
; So this should get resolved by conventional methods.
; At best, the dependency can be a hint that the B5 cell is important to any
; Ai cell where i<5... but I think the simple algorithm will know that naturally.


; Enough explanation, let's get to implementation!

; To analyze a WxH grid, we will create some WxH matrixes to store various
; calculations about each cell.
(define-type (Matrixof A) (Vectorof (Vectorof A)))

(: make-matrix (All (A) (-> Index Index A (Matrixof A))))
(define (make-matrix w h val)
  (build-vector w (lambda (i) (make-vector h val))))

(: matrix-set! (All (A) (-> (Matrixof A) Loc A Void)))
(define (matrix-set! m loc val)
  (let ([col (vector-ref m (loc-x loc))])
    (vector-set! col (loc-y loc) val)))

(: matrix-get (All (A) (-> (Matrixof A) Loc A)))
(define (matrix-get m loc)
  (let ([col (vector-ref m (loc-x loc))])
    (vector-ref col (loc-y loc))))

; Pillar: a vertical sequence of same-colored occupants (non-blank)
(struct pillar ([lo : Loc] ; lower loc
                [hi : Loc] ; higher loc, might be same as lower
                [color : Color]
                [stable? : Boolean])
  #:type-name Pillar
  #:transparent)

(: pillar-contains? (-> Pillar Loc Boolean))
(define (pillar-contains? pillar loc)
  (let* ([lo (pillar-lo pillar)]
         [hi (pillar-hi pillar)])
    (and (= (loc-x lo) (loc-x loc))
         (<= (loc-y lo) (loc-y loc) (loc-y hi)))))

; The dependency [B3 A3] (or "B3 depends on A3") would be stored in this hash
; with a Key of B3 and a Value of A3.
(define-type Deps (Immutable-HashTable Loc Loc))

; The blockage ([B3 A3] [B1 A1]) would be stored in this hash
; with a Key of (cons B3 A3) and a Value of (cons B1 A1)
(define-type Blockages (Immutable-HashTable (Pairof Loc Loc)
                                            (Pairof Loc Loc)))

(struct column-stats
  ([color-mismatch-count : Fixnum] ; how many color mismatches exist?
   [peak : Loc])
  #:type-name ColumnStats
  #:transparent)

(struct analysis ([grid : Grid]
                  [column-stats : (Vectorof ColumnStats)]
                  [pillars : (Matrixof (U #f Pillar))]
                  [dependencies : Deps]
                  [blockages : Blockages])
  #:type-name Analysis
  #:transparent)

(: analysis-pillar (-> Analysis Loc (U #f Pillar)))
(define (analysis-pillar a loc)
  (matrix-get (analysis-pillars a) loc))

(: find-pillars (-> Grid (List (Matrixof (U #f Pillar))
                               (Vectorof ColumnStats))))
(define (find-pillars [grid : Grid])
  (define w (grid-width grid))
  (define h (grid-height grid))
  (define matrix (make-matrix w h (ann #f (U #f Pillar))))
  (define stats : (Listof ColumnStats) '())

  (: pillar-end (-> Integer Integer Color Loc))
  (define (pillar-end x y color)
    (define done (make-loc x (sub1 y)))
    (let* (#:break (when (= y h) done)
           [occ (grid-get grid (make-loc x y))]
           #:break (when (not occ) done)
           [color2 (occupant-color occ)]
           #:break (when (not (equal? color color2)) done))
      (pillar-end x (add1 y) color)))

  (: is-stable? (-> Integer Integer Boolean))
  (define (is-stable? x y)
    (and (y . > . -1)
         (let* ([col (vector-ref matrix x)]
                [pillar (vector-ref col y)])
           (and pillar (pillar-stable? pillar)))))

  (for ([x (in-range w)])
    (define current-pillar : (U #f Pillar) #f)
    (define color-mismatch-counter : (Pairof Color Fixnum) (cons #f 0))
    (define peak : Loc (make-loc x 0))
    (for ([y (in-range h)])
      (let* ([loc (make-loc x y)]
             [occ (grid-get grid loc)]
             [cp current-pillar])
        ; Update column stats
        (when occ
          (set! peak loc)
          (let* ([color (occupant-color occ)])
            (when (and color
                       (not (equal? color (car color-mismatch-counter))))
              (set! color-mismatch-counter
                    (cons color
                          (fx+ 1 (cdr color-mismatch-counter)))))))
        ; Do pillar logic
        (when cp
          (if (pillar-contains? cp loc)
              (matrix-set! matrix loc cp)
              (set! current-pillar #f)))
        (when (and (not current-pillar) occ)
          (let* ([start loc]
                 [color (occupant-color occ)]
                 [end (pillar-end x (add1 y) color)]
                 [stable? (or (fuel? occ)
                              (and (not (can-burst? occ))
                                   (is-stable? x (sub1 y))))])
            (set! current-pillar (pillar start end color stable?))
            (matrix-set! matrix loc current-pillar)))))
    (set! stats (cons (column-stats (cdr color-mismatch-counter) peak)
                      stats)))
  (list matrix
        (list->vector (reverse stats))))

(: find-deps (-> Grid (-> Loc Boolean) Deps))
(define (find-deps grid stable?)
  (define w (grid-width grid))
  (define h (grid-height grid))
  (define deps : Deps (hash))
  (for ([x (in-range w)])
    (for ([y (in-range h)])
      (let* ([loc (make-loc x y)]
             #:break (when (stable? loc) #f)
             [occ (grid-get grid loc)]
             #:break (when (not (catalyst? occ)) #f)
             [dir (catalyst-direction occ)]
             #:break (when (not (member dir '(l r))) #f)
             [loc2 (loc-neighbor loc dir)])
        (set! deps (hash-set deps loc loc2)))))
  deps)

(: find-blockages (-> Grid (Matrixof (U #f Pillar)) Deps Blockages))
(define (find-blockages grid pillar-info deps)
  (define-type Dep (Pairof Loc Loc))

  (define (share-pillar? [a : Loc] [b : Loc])
    (let ([p1 (matrix-get pillar-info a)]
          [p2 (matrix-get pillar-info b)])
      (equal? p1 p2)))

  (: fully-occupied? (-> Loc Loc Boolean))
  (define (fully-occupied? Pi Pj)
    (if (equal? Pi Pj)
        #t
        (and (let ([occ (grid-get grid Pj)])
               (and occ
                    (not (can-burst? occ))))
             (fully-occupied? Pi (loc-neighbor Pj 'u)))))

  (define (blockage? [x : (List Dep Dep)])
    (let* ([d1 (first x)]
           [d2 (second x)]
           [Pi (car d1)]
           [Qi (cdr d1)]
           [Pj (car d2)]
           [Qj (cdr d2)])
      (and (= (loc-x Pi) (loc-x Pj))
           (= (loc-x Qi) (loc-x Qj))
           ((loc-y Pi) . > . (loc-y Pj))
           (not (share-pillar? Qi Qj))
           ; Skip this for now and see if it matters
           #;(fully-occupied? Pi Pj))))

  (let* ([deps : (Listof Dep) (hash->list deps)]
         ; TODO Slow code maybe?
         ; Would it be faster to group deps by column first, and only
         ; then look for blockages?
         [blocks (cartesian-product deps deps)]
         [blocks (filter blockage? blocks)]
         [blocks : (Listof (Pairof Dep Dep))
                 (map (lambda ([x : (List Dep Dep)])
                        (ann (cons (first x) (second x))
                             (Pairof Dep Dep)))
                      blocks)])
    (make-immutable-hash blocks)))

(define (grid-analysis [grid : Grid])
  (define result (find-pillars grid))
  (define pillars (first result))
  (define (stable? [loc : Loc])
    (let* ([pillar (matrix-get pillars loc)])
      (and pillar (pillar-stable? pillar))))
  (define deps (find-deps grid stable?))
  (define blockages (find-blockages grid pillars deps))
  (analysis grid (second result) pillars deps blockages))


; These "check-" functions are just to help the test submodule.
(: check-stability (-> Analysis Loc (U 'stable 'unstable) Boolean))
(define (check-stability analysis loc expected)
  (let* ([pillar (analysis-pillar analysis loc)]
         [actual (and pillar (if (pillar-stable? pillar)
                                 'stable
                                 'unstable))])
    (equal? expected actual)))

(: check-pillar (-> Analysis Loc Loc Boolean))
(define (check-pillar a start end)
  (let ([pillar (analysis-pillar a start)])
    (and pillar
         (equal? start (pillar-lo pillar))
         (equal? end (pillar-hi pillar)))))

(: check-deps (-> Analysis (Listof (Pairof Loc Loc)) Boolean))
(define (check-deps a expected)
  (equal? (analysis-dependencies a)
          (make-immutable-hash expected)))

(: check-blockages (-> Analysis
                       (Listof (Pairof (Pairof Loc Loc)
                                       (Pairof Loc Loc)))
                       Boolean))
(define (check-blockages a expected)
  (equal? (analysis-blockages a)
          (make-immutable-hash expected)))

(module+ test
  (define-syntax-rule (test-helper condition id ...)
    (when (not condition)
      (displayln "\n\n === Test Failure ===")
      (begin
        (displayln (format "value of ~a is" 'id))
        (show id))
      ...
      (fail "Condition failed" 'condition)))

  (define-syntax (check stx)
    (syntax-parse stx #:datum-literals (>)
      [(_ #:true condition:expr id:id ...)
       (syntax/loc stx
         (test-helper condition id ...))]
      [(_ a:id > b:id)
       (syntax/loc stx
         (test-helper ((grid-score a) . > . (grid-score b))
                      a b))]
      [(_ #:stable? analysis:id cell ...)
       (syntax/loc stx
         (check #:stability analysis 'stable {cell ...}))]
      [(_ #:unstable? analysis:id cell ...)
       (syntax/loc stx
         (check #:stability analysis 'unstable {cell ...}))]
      [(_ #:stability analysis:id expected {cell more-cells ...})
       (quasisyntax/loc stx
         (begin
           #,(syntax/loc stx
               (test-helper (check-stability analysis cell expected)
                            analysis))
           #,(syntax/loc stx
               (check #:stability analysis expected {more-cells ...}))))]
      [(_ #:stability analysis expected {})
       (syntax/loc stx (void))]
      [(_ #:pillar? analysis {})
       (syntax/loc stx (void))]
      [(_ #:pillar? analysis:id {[loca locb] more ...})
       (quasisyntax/loc stx
         (begin
           #,(syntax/loc stx
               (test-helper (check-pillar analysis loca locb)
                            analysis))
           #,(syntax/loc stx
               (check #:pillar? analysis {more ...}))))]
      [(_ #:deps analysis:id {[loca locb] ...})
       (syntax/loc stx
         (test-helper (check-deps analysis (list (cons loca locb) ...))
                      analysis))]
      [(_ #:blockages analysis:id {([loc1 loc2] [loc3 loc4]) ...})
       (syntax/loc stx
         (test-helper (check-blockages analysis (list (cons (cons loc1 loc2)
                                                            (cons loc3 loc4))
                                                      ...))
                      analysis))]
      ))

  (define-syntax-rule (make-state pattern #:queue queue)
    (parse-state 'pattern 'queue)))


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
        ; TODO I think this has become "column-has-blank?" now?
        ; Uncommenting this causes silly play:
        #;[(and ((length vr) . < . 4)
                ((length vbr) . > . (length vr)))
           #t]
        [else #f]))))

(: color-mismatch-penalty (-> Fixnum Fixnum))
(define (color-mismatch-penalty [mismatch-count : Fixnum])
  (case mismatch-count
    ((0) 0)
    ((1) -1)
    ((2) -3)
    ((3) -6)
    ((4) -10)
    ((5) -15)
    ((6) -21)
    ((7) -28)
    ((8) -36)
    ((9) -45)
    ((10) -55)
    ((11) -66)
    ((12) -78)
    ((13) -91)
    ((14) -105)
    ((15) -120)
    ((16) -136)
    ((17) -153)
    ((18) -171)
    ((19) -190)
    ((20) -210)
    [else (fail "unexpected mismatch-count" mismatch-count)]))

(define explain-score? : (Parameterof Boolean) (make-parameter #f))

(: grid-score (-> Grid Fixnum))
(define (grid-score grid)
  (let* ([width (grid-width grid)]
         [(grid-a _)
          (grid-resolve grid '())]
         [analysis-a (grid-analysis grid-a)]
         [column-stats-a (analysis-column-stats analysis-a)]
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
         [analysis-b (grid-analysis grid-b)]
         [column-stats-b (analysis-column-stats analysis-b)]
         [breathing-room-score
          (fxsum
           (for/list ([x (in-range width)])
             (let* ([stats (vector-ref column-stats-a x)]
                    [y (loc-y (column-stats-peak stats))]
                    [y (- (grid-height grid) y)])
               (case y
                 [(0) (fail "impossible:" y)]
                 [(1) 0]
                 [(2) 1000]
                 [(3) 3000]
                 [(4) 6000]
                 [else 10000]))))]
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
                 [(1) -20]
                 [(2) -14]
                 [(3) -4]
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
         [mismatch-score
          (fxsum
           (for/list ([x (in-range width)])
             (let* ([stats (vector-ref column-stats-b x)]
                    [count (column-stats-color-mismatch-count stats)])
               (color-mismatch-penalty count))))]
         [analysis-b (grid-analysis grid-b)]
         [blockage-score
          (fx* -10000 (hash-count (analysis-blockages analysis-b)))]
         [score (fxsum (list burst-score run-score
                             mismatch-score breathing-room-score
                             problem-score blockage-score
                             fuel-score broken-peak-score))])
    (when (explain-score?)
      (println (list "burst:" burst-score
                     "run:" run-score
                     "mismatch:" mismatch-score
                     "breathing room:" breathing-room-score
                     "problem:" problem-score
                     "blockage:" blockage-score
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
                          [.. .. ..]
                          [.. .. ..]
                          [.. .. ..]
                          [.. <o r>]
                          [.. YY ..]
                          [.. YY RR]
                          [.. YY RR]))]
        [g2 (parse-grid '([.. .. ..]
                          [.. .. ..]
                          [.. .. ..]
                          [.. .. ..]
                          [.. .. o^]
                          [.. YY r_]
                          [.. YY RR]
                          [.. YY RR]))]
        [g3 (parse-grid '([.. .. ..]
                          [.. .. ..]
                          [.. .. ..]
                          [.. .. ..]
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
        ; g1 locks in a triple combo; g2 just locks in a single group
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

  ; TODO fix and uncomment this one
  #;(let ([g1 (parse-grid '([.. .. .. ..]
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

  ; TODO fix and uncomment this one
  #;(let ([g1 (parse-grid '([.. .. .. ..]
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

  ; Never create a blockage!!
  (let ([g1 (parse-grid '([.. .. .. ..]
                          [.. .. .. ..]
                          [.. bb .. ..]
                          [<y b> .. ..]
                          [YY .. .. ..]
                          [.. BB <y b>]))]
        [g2 (parse-grid '([.. .. .. ..]
                          [<y b> .. ..]
                          [.. bb .. ..]
                          [<y b> .. ..]
                          [YY .. .. ..]
                          [.. BB .. ..]))])
    (check g1 > g2))

  (let ([g1 (parse-grid '([<y b> .. .. ..]
                          [<r b> .. .. ..]
                          [<r b> .. .. ..]
                          [o^ .. .. o^ ..]
                          [r_ .. .. b_ ..]
                          [RR .. .. BB ..]
                          [YY BB .. BB ..]))]
        ; g1 uses the <y half; g2 wastes it
        [g2 (parse-grid '([.. .. .. .. ..]
                          [<r b> .. .. ..]
                          [<r b> .. <b y>]
                          [o^ .. .. o^ ..]
                          [r_ .. .. b_ ..]
                          [RR .. .. BB ..]
                          [YY BB .. BB ..]))])
    (check g1 > g2))

  (let ([g1 (parse-grid '((.. .. .. .. .. .. .. ..)
                          (.. .. .. .. .. .. .. ..)
                          (.. .. .. .. .. .. .. ..)
                          (.. .. .. .. .. .. .. ..)
                          (.. .. .. .. .. .. .. ..)
                          (.. .. .. .. .. <y b> ..)
                          (.. .. .. .. .. .. <b r>)
                          (.. <r r> y^ .. .. <b r>)
                          (.. <o r> y_ .. <b y> ..)
                          (.. RR <o y> .. b^ .. ..)
                          (.. .. RR .. .. o_ .. ..)
                          (.. RR RR .. .. BB .. RR)
                          (.. .. .. YY rr BB .. ..)
                          (.. .. RR .. rr .. .. RR)
                          (-- -- -- -- -- -- -- --)))]
        [g2 (parse-grid '((.. .. .. .. .. .. .. ..)
                          (.. .. .. .. .. .. .. ..)
                          (.. .. .. .. .. .. .. ..)
                          (.. .. .. .. .. .. .. ..)
                          (.. .. .. .. .. .. y^ ..)
                          (.. .. .. .. .. .. b_ ..)
                          (.. .. .. .. .. .. <b r>)
                          (.. <r r> y^ .. .. <b r>)
                          (.. <o r> y_ .. <b y> ..)
                          (.. RR <o y> .. b^ .. ..)
                          (.. .. RR .. .. o_ .. ..)
                          (.. RR RR .. .. BB .. RR)
                          (.. .. .. YY rr BB .. ..)
                          (.. .. RR .. rr .. .. RR)
                          (-- -- -- -- -- -- -- --)))])
    (check g1 > g2))

  (let ([g1 (parse-grid '((.. .. .. .. .. .. .. ..)
                          (.. .. .. .. .. .. .. ..)
                          (.. .. <y y> .. .. .. ..)
                          (<b o> <o y> b^ .. .. ..)
                          (.. <b y> .. b_ .. .. ..)
                          (.. <r o> .. b^ .. .. y^)
                          (<b r> .. .. b_ .. .. y_)
                          (y^ .. .. .. b^ r^ .. y^)
                          (o_ .. .. .. r_ r_ .. r_)
                          (yy .. YY .. <r y> .. r^)
                          (yy .. .. .. <r y> .. b_)
                          (YY rr .. <y b> .. <r b>)
                          (.. rr .. yy .. .. r^ ..)
                          (.. rr .. YY .. yy r_ ..)
                          (-- -- -- -- -- -- -- --)))]
        [g2 (parse-grid '((.. .. .. .. .. .. .. ..)
                          (.. .. .. .. .. .. .. ..)
                          (.. .. <y y> .. .. .. ..)
                          (<b o> <o y> .. .. .. ..)
                          (.. <b y> .. .. .. .. ..)
                          (.. <r o> .. b^ .. .. y^)
                          (<b r> .. .. b_ .. .. y_)
                          (y^ .. .. .. b^ r^ .. y^)
                          (o_ .. .. .. r_ r_ .. r_)
                          (yy .. YY .. <r y> b^ r^)
                          (yy .. .. .. <r y> b_ b_)
                          (YY rr .. <y b> .. <r b>)
                          (.. rr .. yy .. .. r^ ..)
                          (.. rr .. YY .. yy r_ ..)
                          (-- -- -- -- -- -- -- --)))])
    (check g1 > g2))

  (let* ([g1 (parse-grid '((.. .. .. .. .. .. .. ..)
                           (.. .. .. .. .. .. .. ..)
                           (.. .. .. <y b> .. .. ..)
                           (.. .. .. <y r> .. .. ..)
                           (.. .. <y o> .. .. .. ..)
                           (.. .. y^ .. .. .. .. ..)
                           (r^ .. b_ .. .. <o y> ..)
                           (b_ <o b> .. .. <r y> ..)
                           (b^ b^ .. .. r^ b^ r^ ..)
                           (b_ y_ .. .. o_ b_ r_ ..)
                           (b^ yy bb .. <r b> r^ ..)
                           (y_ bb BB YY <r y> b_ ..)
                           (y^ bb YY .. RR .. <b o>)
                           (y_ bb YY .. .. .. bb ..)
                           (-- -- -- -- -- -- -- --)))]
         [g1 (or (grid-burst g1) g1)]
         [(g1 groups) (grid-resolve g1 '())]
         [g2 (parse-grid '((.. .. .. .. .. .. .. ..)
                           (.. .. .. .. .. .. .. ..)
                           (.. .. .. <y b> .. .. ..)
                           (.. .. .. <y r> .. .. ..)
                           (.. .. <y o> .. .. .. ..)
                           (.. .. y^ .. .. .. .. ..)
                           (b^ .. b_ .. .. <o y> ..)
                           (r_ <o b> .. .. <r y> ..) ; WTF in leftmost column??
                           (b^ b^ .. .. r^ b^ r^ ..)
                           (b_ y_ .. .. o_ b_ r_ ..)
                           (b^ yy bb .. <r b> r^ ..)
                           (y_ bb BB YY <r y> b_ ..)
                           (y^ bb YY .. RR .. <b o>)
                           (y_ bb YY .. .. .. bb ..)
                           (-- -- -- -- -- -- -- --)))])
    (check g1 > g2))
  )
