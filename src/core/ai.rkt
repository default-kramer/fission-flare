#lang typed/racket

(provide (rename-out [get-plan choose-move]))
(module+ exercise-help
  (provide fast-forward))

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
;   - This point is not necessary but it slightly improves the AI's play
;
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
           {([E7 D7] [E5 D5])})))
; That seems reasonable.
; The E5-E7 pillar is the critical point and should be destroyed ASAP.
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

; This parameter enables tuning for the different game modes
(define optimize-for : (Parameterof (U #f 'mini 'standard 'wave))
  (make-parameter #f))

(: param-is (All (A) (-> (Parameterof A) A * Boolean)))
; more type-safe way to test a parameter value
(define (param-is p . vals)
  (and (member (p) vals) #t))

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

(: pillar-length (-> Pillar Fixnum))
(define (pillar-length p)
  (fx+ 1 (fx- (loc-y (pillar-hi p))
              (loc-y (pillar-lo p)))))

(: pillar-locs (-> Pillar (Listof Loc)))
(define (pillar-locs p)
  (let* ([lo (pillar-lo p)]
         [hi (pillar-hi p)]
         [x (loc-x lo)])
    (for/list ([y (in-range (loc-y lo) (fx+ 1 (loc-y hi)))])
      (make-loc x y))))

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
                  [pillar-list : (Listof Pillar)]
                  [dependencies : Deps]
                  [blockages : Blockages]
                  [problematic-proc : Problematic?])
  #:type-name Analysis
  #:transparent)

(: analysis-pillar (-> Analysis Loc (U #f Pillar)))
(define (analysis-pillar a loc)
  (matrix-get (analysis-pillars a) loc))

(: column-danger (-> Analysis Integer Fixnum))
(define (column-danger analysis x)
  (let* ([stats (analysis-column-stats analysis)]
         [stats (vector-ref stats x)]
         [peak (column-stats-peak stats)]
         [pillar (analysis-pillar analysis peak)]
         #:break (when (not pillar) 0)
         ; We are considering the shattered grid, so don't check stability
         ; ... Is what I *would* think, but for some reason standard layout
         ; still benefits from this check. Not sure why.
         #:break (when (and (param-is optimize-for 'standard)
                            (not (pillar-stable? pillar)))
                   0)
         ; assuming group of 4 will destroy:
         [needed (fx- 4 (pillar-length pillar))]
         [height (grid-height (analysis-grid analysis))]
         ; TODO the grid has 2 empty top rows, should redo this
         ; for grep: mover-reserved-rows
         [height (fx- height 2)]
         [space (fx- height (loc-y peak))]
         [room (fx- space needed)])
    (if (room . fx< . 1)
        -1001001
        (case room
          [(1) -3000]
          [(2) -2000]
          [(3) -1000]
          [else 0]))))

(: find-pillars (-> Grid (List (Matrixof (U #f Pillar))
                               (Vectorof ColumnStats)
                               (Listof Pillar))))
(define (find-pillars [grid : Grid])
  (define w (grid-width grid))
  (define h (grid-height grid))
  (define matrix (make-matrix w h (ann #f (U #f Pillar))))
  (define all-pillars : (Listof Pillar) '())
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
                              (ground? occ)
                              (and (not (can-burst? occ))
                                   (is-stable? x (sub1 y))))])
            (let ([p (pillar start end color stable?)])
              (set! current-pillar p)
              (set! all-pillars (cons p all-pillars)))
            (matrix-set! matrix loc current-pillar)))))
    (set! stats (cons (column-stats (cdr color-mismatch-counter) peak)
                      stats)))
  (list matrix
        (list->vector (reverse stats))
        all-pillars))

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
           ; Everything seems to work fine without the following check,
           ; but it does slightly improve both mini and standard
           (fully-occupied? Pi Pj))))

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
  (define problematic-proc (make-problematic-proc grid))
  (analysis grid (second result) pillars (third result) deps blockages
            problematic-proc))


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


(: grid-resolve (->* (Grid (Listof DestructionGroup))
                     ((U 'vertical 'horizontal 'both))
                     (Values Grid (Listof DestructionGroup))))
; Fast-forward the "Fall -> Destroy -> Fall -> Destroy ..." cycle.
; Return resulting grid and all destruction groups.
(define (grid-resolve grid accum [mode 'both])
  (let* ([g2 (grid-fall grid)]
         #:break (when g2
                   (grid-resolve g2 accum mode))
         [(g2 groups)
          (grid-destroy grid #:mode mode)])
    (if g2
        (grid-resolve g2 (append groups accum) mode)
        (values grid accum))))

(: grid-shatter (-> (U Analysis Grid)
                    (Listof DestructionGroup)
                    (Values Grid Analysis (Listof DestructionGroup))))
; For each pillar that is a stable peak, separate all joined catalysts
; and run the fall+destroy cycle to completion.
; Repeat this entire process by recursion until nothing changes.
; Doing this may "shake loose" some unstable pillars causing them to fall
; and become stable or even become destroyed.
; The resulting grid is a prediction only; it is not an inevitable outcome.
; Even so, it seems to be a good-enough heuristic.
(define (grid-shatter x groups)

  (: change-color (-> Occupant Occupant))
  ; After a pillar is shattered, we change its color to reflect the idea
  ; that it wouldn't even be on the grid if it was truly destroyed.
  ; (If we didn't do this, these "ghost pillars" might color-match with
  ;  real occupants and cause implausible destructions.)
  ; Perhaps a better alternative would be to simply destroy the pillar
  ; that is being shattered, but we would need to update other code to
  ; avoid rewarding this un-earned destruction.
  (define (change-color occ)
    (define (get-color)
      (case (occupant-color occ)
        [(r r2) 'r2]
        [(y y2) 'y2]
        [(b b2) 'b2]
        [else (fail "unexpected color" occ)]))
    (cond
      [(catalyst? occ)
       (struct-copy catalyst occ [direction #f] [color (get-color)])]
      [(fuel? occ)
       (struct-copy fuel occ [color (get-color)])]
      [(ground? occ)
       occ]
      [else
       (fail "unexpected occupant in pillar" occ)]))

  (let* ([a : Analysis (if (grid? x)
                           (grid-analysis x)
                           x)]
         [grid : Grid (analysis-grid a)]
         [stats (analysis-column-stats a)]
         [mod! (grid-modifier grid)]
         [done : (Listof Loc) '()]
         [found? : Boolean #f])
    (for ([x (in-range (grid-width grid))])
      (let* ([stats (vector-ref stats x)]
             [peak (column-stats-peak stats)]
             [pillar (analysis-pillar a peak)]
             #:break (when (not pillar) #f)
             ; Only shatter stable pillars
             #:break (when (not (pillar-stable? pillar)) #f))
        (for ([loc (pillar-locs pillar)])
          (set! done (cons loc done))
          (let* ([occ (or (grid-get grid loc)
                          (fail "pillar has empty loc" loc pillar))]
                 [_ (mod! 'remove loc)]
                 [_ (mod! 'set loc (change-color occ))]
                 #:break (when (not (catalyst? occ)) #f)
                 [dir (catalyst-direction occ)]
                 #:break (when (not (member dir '(l r)))
                           #f)
                 [loc2 (loc-neighbor loc dir)]
                 #:break (when (member loc2 done) #f)
                 [occ2 (grid-get grid loc2)]
                 #:break (when (not (catalyst? occ2))
                           (fail "invalid catalyst pair" loc occ grid))
                 [occ (struct-copy catalyst occ [direction #f])]
                 [occ2 (struct-copy catalyst occ2 [direction #f])])
            (set! found? #t)
            (mod! 'remove loc2)
            (mod! 'set loc2 occ2)))))
    (if found?
        (let* ([grid (or (mod! 'build)
                         (fail "shatter failed"))]
               [(grid groups)
                ; Including horizontal groups can cause bad moves.
                ; If you are curious, there is at least one instructive test
                ; that will fail if you change this.
                (grid-resolve grid groups 'vertical)])
          (grid-shatter grid groups))
        (values grid a groups))))

(define-type Problematic? (-> (U Loc Pillar) Boolean))

(: make-problematic-proc (-> Grid Problematic?))
; Constructs the `problematic?` procedure.
; Imprecise: A location is problematic if it contains fuel or
; if it blocks vertical access to fuel.
; Precise: A location is problematic if any of these is true:
; * It contains fuel
; * It is above a problematic location (in the same column)
; * It contains a catalyst whose partner satisfies *all* of these:
;   1) direction is left or right
;   2) has an empty space below
;   3) is above a location
;
; If any location in a pillar `Px` is problematic, the top of that pillar `Pt`
; is guaranteed to be problematic because
#;(or (equal? Pt Px) (Pt . is-above? . Px))
; is always true.
; Therefore a pillar is problematic iff its top location is problematic.
(define (make-problematic-proc grid)
  (define w (grid-width grid))
  (define h (grid-height grid))
  (define cache (make-matrix w h (ann 'unset (U Boolean 'unset))))

  (: get-result (-> Integer Integer Boolean))
  (define (get-result x y)
    (define (key-point? [x : Integer] [y : Integer])
      (and (not (grid-get grid (make-loc x y)))
           (problematic? (make-loc x (sub1 y)))))
    (let ([occ (grid-get grid (make-loc x y))])
      (cond
        [(fuel? occ)
         #t]
        [(y . < . 1)
         #f]
        [(problematic? (make-loc x (sub1 y)))
         #t]
        [(catalyst? occ)
         (case (catalyst-direction occ)
           [(l) (key-point? (sub1 x) (sub1 y))]
           [(r) (key-point? (add1 x) (sub1 y))]
           [else #f])]
        [else #f])))

  (: problematic? (-> Loc Boolean))
  (define (problematic? loc)
    (let ([result (matrix-get cache loc)])
      (case result
        [(#t) #t]
        [(#f) #f]
        [(unset)
         (let ([result (get-result (loc-x loc) (loc-y loc))])
           (matrix-set! cache loc result)
           result)])))

  (lambda ([x : (U Loc Pillar)])
    (let ([loc (if (pillar? x)
                   (pillar-hi x)
                   x)])
      (and (in-bounds? grid loc)
           (problematic? loc)))))

(module+ test
  (let ([a (grid-analyze
            [17 .. .. .. .. .. ..]
            [16 .. .. .. .. .. ..]
            [15 .. .. .. .. .. ..]
            [14 .. .. .. .. .. ..]
            [13 .. .. .. .. .. ..]
            [12 .. .. .. .. .. ..]
            [11 .. yy .. .. .. ..]
            [10 .. rr .. .. .. ..]
            [-9 .. rr .. .. .. ..]
            [-8 .. rr .. .. .. ..]
            [-7 <y y> .. .. .. ..]
            [-6 .. y^ .. .. .. ..]
            [-5 .. r_ <b r> .. ..]
            [-4 yy b^ .. <r y> ..]
            [-3 yy y_ .. rr .. ..]
            [-2 YY y^ b^ bb .. ..]
            [-1 BB y_ b_ bb .. ..]
            [-- A: B: C: D: E: F:])])
    (let ([problematic? (analysis-problematic-proc a)])
      (check #:true (problematic? B11))
      (check #:true (not (problematic? E4))))))

(: fxsum (-> (Listof Fixnum) Fixnum))
(define (fxsum nums)
  (for/fold ([sum : Fixnum 0])
            ([num nums])
    (fx+ sum num)))

(: color-mismatch-penalty (-> Fixnum Fixnum))
(define (color-mismatch-penalty [mismatch-count : Fixnum])
  (case mismatch-count
    ((0 1) 0)
    ((2) -1000)
    ((3) -3000)
    ((4) -6000)
    ((5) -10000)
    ((6) -15000)
    ((7) -21000)
    ((8) -28000)
    ((9) -36000)
    ((10) -45000)
    ((11) -55000)
    ((12) -66000)
    ((13) -78000)
    ((14) -91000)
    ((15) -105000)
    ((16) -120000)
    ((17) -136000)
    ((18) -153000)
    ((19) -171000)
    ((20) -190000)
    ((21) -210000)
    [else (fail "unexpected mismatch-count" mismatch-count)]))

(define explain-score? : (Parameterof Boolean) (make-parameter #f))

(: score (-> (U State Grid Possibility) Fixnum))
(define (score x)
  (grid-score (cond
                [(grid? x) x]
                [(state? x) (state-grid x)]
                [else (state-grid (car x))])))

(: score-pillars (-> Analysis Fixnum))
(define (score-pillars a)
  (let ([pillars (analysis-pillar-list a)]
        [problematic? (analysis-problematic-proc a)])
    (fxsum
     (for/list ([p pillars])
       (if (problematic? p)
           (begin
             (case (pillar-length p)
               ; Ending these with a 1 means the last 2 digits of the sum
               ; should usually be the count of problematic pillars
               [(1) -6001] ; 1 -> 2 improves by 2000
               [(2) -4001] ; 2 -> 3 improves by 3000
               [(3) -1001] ; 3 -> 4 improves by 1000
               [else (fail "undestroyed group" p)]))
           (case (pillar-length p)
             [(1) -600]
             [(2) -400]
             [(3) -100]
             [else (fail "undestroyed group" p)]))))))

(: favor-higher-pillars (-> Analysis Fixnum))
(define (favor-higher-pillars a)
  (let ([grid (analysis-grid a)]
        [pillars (analysis-pillar-list a)])
    (fxsum
     (for/list ([p pillars])
       (let* ([y (loc-y (pillar-lo p))])
         (fx* y (case (pillar-length p)
                  [(1) -3]
                  [(2) -2]
                  [(3) -1]
                  [else 0])))))))

(: score-blanks (-> Grid Fixnum))
(define (score-blanks grid)
  (define (check-bonus [loc : Loc] [dir : Direction])
    (let* ([loc2 (loc-neighbor loc dir)]
           [loc3 (loc-neighbor loc2 'd)])
      (ann (if (not (grid-get grid loc3))
               10
               1) Fixnum)))
  (let* ([sum : Fixnum 0])
    (for ([x (in-range (grid-width grid))])
      (for ([y (in-range (grid-height grid))])
        (let* ([loc (make-loc x y)]
               [occ (grid-get grid loc)]
               #:break (when (not (catalyst? occ)) #f)
               #:break (when (catalyst-color occ) #f)
               [dir (catalyst-direction occ)]
               [adder : Fixnum (case dir
                                 [(#f u d) 1]
                                 [(l r) (check-bonus loc dir)])])
          (set! sum (fx+ adder sum)))))
    sum))

(: grid-score (-> Grid Fixnum))
(define (grid-score grid-a)
  (let* ([width (grid-width grid-a)]
         [height (grid-height grid-a)]
         [blank-score (score-blanks grid-a)]
         [bursted? (grid-burst grid-a)]
         [(grid-a-resolved groups-a)
          (grid-resolve grid-a '())]
         [grid-b (or bursted? grid-a)]
         [(grid-b groups-b)
          (grid-resolve grid-b '())]
         [analysis-b (grid-analysis grid-b)]
         [column-stats-b (analysis-column-stats analysis-b)]
         [blockage-score
          (fx* -2002002 (hash-count (analysis-blockages analysis-b)))]
         [(grid-c analysis-c groups-c)
          (grid-shatter analysis-b '())]
         [pillar-score (score-pillars analysis-c)]
         [pillar-height-score (favor-higher-pillars analysis-c)]
         [groups-b-score
          (fx* (if bursted? 5001 101)
               (fx- (length groups-b) (length groups-a)))]
         [groups-c-score
          (fx* 101 (cast (length groups-c) Fixnum))]
         [mismatch-score
          (fxsum
           (for/list ([x (in-range width)])
             (let* ([stats (vector-ref column-stats-b x)]
                    [count (column-stats-color-mismatch-count stats)])
               (color-mismatch-penalty count))))]
         [danger-score
          (let ([a (if (param-is optimize-for 'standard)
                       analysis-b
                       analysis-c)])
            (fxsum (for/list ([x (in-range width)])
                     (column-danger a x))))]
         ; == Notes on breathing-room-score ==
         ; Data collected 2022-02-12
         ; Using grid-a-resolved
         ; * mini is 11209 (best so far)
         ; * standard got much worse (vs disabled)
         ; Using column-stats-b
         ; * mini drops from 11209 to 11147 (vs grid-a-resolved)
         ; * standard improves by only 1 point (vs disabled)
         ; When disabled
         ; * mini drops from 11209 to 11145 (vs grid-a-resolved)
         ; * standard is 27921 (best so far)
         [breathing-room-score
          (if (param-is optimize-for 'mini)
              (let* ([analysis (grid-analysis grid-a-resolved)]
                     [column-stats (analysis-column-stats analysis)])
                (fxsum
                 (for/list ([x (in-range width)])
                   (let* ([stats (vector-ref column-stats x)]
                          [y (loc-y (column-stats-peak stats))]
                          [y (- height y)])
                     (case y
                       [(0) (fail "impossible:" y)]
                       [(1) 0]
                       [(2) 1000]
                       [(3) 3000]
                       [(4) 6000]
                       [else 10000])))))
              0)])
    (define-syntax-rule (do-total id ...)
      (let ([total (fxsum (list id ...))])
        (when (explain-score?)
          (pretty-print (list (cons 'id id) ...)))
        total))
    (do-total pillar-score
              pillar-height-score
              blank-score
              groups-b-score
              groups-c-score
              breathing-room-score
              mismatch-score
              danger-score
              blockage-score)))

(define-type MoveSeq (Listof Action))

; A Possibility (or "possi") is a reachable state.
; (Hmm... perhaps we should also hold the starting state?)
(define-type Possibility (List State MoveSeq))
(define possi-state (ann first (-> Possibility State)))

; A Choice is a Possibility that has been scored
(define-type Choice (Pairof Fixnum Possibility))
(define choice-score (ann first (-> Choice Fixnum)))
(define choice-state (ann second (-> Choice State)))
(define choice-possi (ann cdr (-> Choice Possibility)))

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

(: foo (-> (U Action (Listof Action)) Boolean (-> Possibility (Listof Possibility))))
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

(: catalyst-positions (-> State (Listof Possibility)))
; Returns all possible positions for the catalyst, but do not decide yet
; whether we will plummet or burst.
(define (catalyst-positions state)
  (let ([rots (get-rotations state)])
    (append rots
            (flatmap (foo (move 'l) #t) rots)
            (flatmap (foo (move 'r) #t) rots))))

(: fast-forward (-> State State))
(define (fast-forward state)
  (let* ([result (state-apply state (list (tick)))]
         [new-state (car result)])
    (if new-state
        (fast-forward new-state)
        state)))

(: choose-single-move (-> State (U 'plummet 'burst) (U #f Choice)))
; Evaluates all possible moves and returns the best-scoring one
(define (choose-single-move state kind)
  (let* ([state (fast-forward state)]
         #:break (when (state-game-over? state) #f)
         [actions (case kind
                    [(plummet) (list (plummet))]
                    [(burst) (list (plummet) (burst))])]
         [possis (flatmap (foo actions #f) (catalyst-positions state))]
         [choices (map (lambda ([p : Possibility])
                         (ann (cons (score p) p) Choice))
                       possis)]
         #:break (when (empty? choices) #f)
         [car* (ann car (-> Choice Fixnum))])
    (argmax car* choices)))

(: count-groups (-> State Integer))
(define (count-groups state)
  (let* ([(grid groups)
          (grid-resolve (state-grid state) '())])
    (length groups)))

(: get-plan (-> State (U #f Possibility)))
(define (get-plan state)
  (let* ([settings (state-settings state)]
         [mode (game-settings-layout:mode settings)])
    (parameterize ([optimize-for mode])
      (get-plan2 state))))

(: get-plan2 (-> State (U #f Possibility)))
(define (get-plan2 state)
  ; Naming convention for this function:
  ;  p means "ply 1 plummet"
  ;  b means "ply 1 burst"
  ;  pb means "ply 1 plummet, then ply 2 burst"
  ;  bp means "ply 1 burst, then ply 2 plummet"
  ; If b is impossible we know that p is our only choice.
  ; If bp outscores pb that means that we should burst in ply 1 because it
  ; will allow us to make a better move in ply 2.
  ; Otherwise bursting in ply 1 is premature.
  (let* ([p (choose-single-move state 'plummet)]
         #:break (when (not p) #f)
         [b (try (choice-possi p) (burst))]
         #:break (when (not b)
                   ; Can't burst in ply 1, just plummet:
                   (choice-possi p))
         [p : Choice p] ; we needed to score p in order to choose it
         [b : Possibility b] ; but we might never need to score b
         ; If p has any destruction groups and b has even more, use b
         [p-group-count (count-groups (choice-state p))]
         [b-group-count (count-groups (possi-state b))]
         #:break (when (and (p-group-count . > . 0)
                            (b-group-count . > . p-group-count))
                   b)
         [b-state (possi-state b)]
         [b-state (fast-forward b-state)]
         #:break (when (case (state-game-over? b-state)
                         [(win) #t]
                         [else #f])
                   ; A ply 1 burst wins the game, do it:
                   b)
         [pb (choose-single-move (choice-state p) 'burst)]
         [bp (choose-single-move (possi-state b) 'plummet)])
    (if (and pb bp)
        ; This return value discards the 2nd ply analysis because I don't
        ; want to change the calling code right now. This is an obvious
        ; potential performance improvement.
        ; (In fact, maybe `get-plan` should keep choosing moves until it
        ;  chooses a burst.)
        (let ([diff (- (choice-score bp) (choice-score pb))])
          (if (diff . > . 200)
              (begin ;(println (format "proactive burst benefit: ~a" diff))
                b)
              (choice-possi p)))
        (if ((score b) . > . (choice-score p))
            b
            (choice-possi p)))))

(module+ test
  (let ([g1 (parse-grid '([.. .. .. ..]
                          [.. .. .. ..]
                          [.. .. .. ..]
                          [.. .. .. ..]
                          [.. .. .. ..]
                          [.. .. .. ..]
                          [.. .. .. ..]
                          [.. .. .. ..]
                          [.. .. .. ..]
                          [.. .. .. ..]
                          [<y y> .. ..]
                          [YY YY .. YY]
                          [.. .. .. YY]
                          [.. .. .. YY]))]
        [g2 (parse-grid '([.. .. .. ..]
                          [.. .. .. ..]
                          [.. .. .. ..]
                          [.. .. .. ..]
                          [.. .. .. ..]
                          [.. .. .. ..]
                          [.. .. .. ..]
                          [.. .. .. ..]
                          [.. .. .. ..]
                          [.. y^ .. ..]
                          [.. y_ .. ..]
                          [YY YY .. YY]
                          [.. .. .. YY]
                          [.. .. .. YY]))]
        ; This group of 5 wastes 1 catalyst:
        [g3 (parse-grid '([.. .. .. ..]
                          [.. .. .. ..]
                          [.. .. .. ..]
                          [.. .. .. ..]
                          [.. .. .. ..]
                          [.. .. .. ..]
                          [.. .. .. ..]
                          [.. .. .. ..]
                          [.. .. .. ..]
                          [.. .. .. y^]
                          [.. .. .. y_]
                          [YY YY .. YY]
                          [.. .. .. YY]
                          [.. .. .. YY]))])
    (check g1 > g3)
    (check g2 > g3))

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

  (let ([g1 (parse-grid '((.. .. .. .. .. .. .. ..)
                          (.. .. .. .. .. .. .. ..)
                          (.. .. .. .. .. .. .. ..)
                          (.. .. .. .. .. .. .. ..)
                          (.. .. .. .. .. .. .. ..)
                          (.. b^ .. .. .. .. .. ..)
                          (.. r_ .. .. .. .. .. ..)
                          (.. <r o> .. .. .. .. ..)
                          (<b r> .. .. .. .. .. ..)
                          (bb .. .. .. .. .. .. BB)
                          (BB .. .. y^ .. .. .. ..)
                          (YY .. .. y_ .. .. .. BB)
                          (RR rr .. YY .. <b y> YY)
                          (BB BB .. .. .. <b y> ..)
                          (.. BB YY YY .. BB .. RR)
                          (RR .. BB .. .. YY .. BB)
                          (BB .. RR YY .. BB YY YY)
                          (YY RR RR YY .. RR BB RR)
                          (RR BB .. BB .. RR .. ..)
                          (BB RR BB .. YY BB YY RR)
                          (.. .. BB YY RR RR .. RR)
                          (-- -- -- -- -- -- -- --)))]
        ; Regression: the AI was correctly penalizing the blockage in g2 but
        ; not enough to overcome other factors.
        [g2 (parse-grid '((.. .. .. .. .. .. .. ..)
                          (.. .. .. .. .. .. .. ..)
                          (.. .. .. .. .. .. .. ..)
                          (.. .. .. .. .. .. .. ..)
                          (.. .. .. .. .. .. .. ..)
                          (.. .. .. .. .. .. .. ..)
                          (<b r> .. .. .. .. .. ..)
                          (.. <r o> .. .. .. .. ..)
                          (<b r> .. .. .. .. .. ..)
                          (bb .. .. .. .. .. .. BB)
                          (BB .. .. y^ .. .. .. ..)
                          (YY .. .. y_ .. .. .. BB)
                          (RR rr .. YY .. <b y> YY)
                          (BB BB .. .. .. <b y> ..)
                          (.. BB YY YY .. BB .. RR)
                          (RR .. BB .. .. YY .. BB)
                          (BB .. RR YY .. BB YY YY)
                          (YY RR RR YY .. RR BB RR)
                          (RR BB .. BB .. RR .. ..)
                          (BB RR BB .. YY BB YY RR)
                          (.. .. BB YY RR RR .. RR)
                          (-- -- -- -- -- -- -- --)))])
    (check g1 > g2))

  (let ([g1 (parse-grid '([.. .. .. ..]
                          [.. .. .. ..]
                          [.. .. .. ..]
                          [.. .. .. ..]
                          [.. .. .. ..]
                          [.. .. <y r>]
                          [.. .. .. rr]
                          [.. .. .. rr]
                          [.. .. .. bb]
                          [.. .. YY BB]
                          [RR YY .. ..]
                          [YY RR BB YY]))]
        ; Even though we are not immediately clearing the problem on the right
        ; side, it is still important to make progress.
        ; (The next test contains a real-game example of this.)
        [g2 (parse-grid '([.. .. .. ..]
                          [.. .. .. ..]
                          [.. .. .. ..]
                          [.. .. .. ..]
                          [.. .. .. ..]
                          [.. .. .. ..]
                          [.. .. .. rr]
                          [.. .. .. rr]
                          [.. .. .. bb]
                          [<r y> YY BB]
                          [RR YY .. ..]
                          [YY RR BB YY]))])
    (check g1 > g2))

  (let ([g1 (parse-grid '((.. .. .. .. .. .. .. ..)
                          (.. .. .. .. .. .. .. ..)
                          (.. .. .. .. .. .. .. ..)
                          (.. .. .. .. .. .. .. ..)
                          (.. .. .. .. .. .. .. ..)
                          (.. .. <y r> .. .. .. ..)
                          (.. .. .. rr .. .. .. ..)
                          (.. .. .. r^ .. .. .. ..)
                          (.. r^ .. y_ .. .. bb ..)
                          (.. b_ .. YY .. .. bb ..)
                          (.. bb .. .. .. YY BB ..)
                          (.. bb .. RR .. RR YY ..)
                          (.. rr .. YY RR BB YY bb)
                          (BB rr .. .. YY RR BB bb)
                          (YY BB YY .. BB BB YY BB)
                          (BB .. BB RR YY RR YY RR)
                          (YY BB BB RR RR BB RR YY)
                          (RR YY RR YY RR YY BB RR)
                          (BB .. BB YY YY RR .. ..)
                          (RR BB YY .. RR YY BB BB)
                          (BB .. RR RR .. .. .. ..)
                          (-- -- -- -- -- -- -- --)))]
        [g2 (parse-grid '((.. .. .. .. .. .. .. ..)
                          (.. .. .. .. .. .. .. ..)
                          (.. .. .. .. .. .. .. ..)
                          (.. .. .. .. .. .. .. ..)
                          (.. .. .. .. .. .. .. ..)
                          (.. .. .. .. .. .. .. ..)
                          (.. .. .. rr .. .. .. ..)
                          (.. .. .. r^ .. .. .. ..)
                          (.. r^ .. y_ .. .. bb ..)
                          (.. b_ .. YY <r y> bb ..)
                          (.. bb .. .. .. YY BB ..)
                          (.. bb .. RR .. RR YY ..)
                          (.. rr .. YY RR BB YY bb)
                          (BB rr .. .. YY RR BB bb)
                          (YY BB YY .. BB BB YY BB)
                          (BB .. BB RR YY RR YY RR)
                          (YY BB BB RR RR BB RR YY)
                          (RR YY RR YY RR YY BB RR)
                          (BB .. BB YY YY RR .. ..)
                          (RR BB YY .. RR YY BB BB)
                          (BB .. RR RR .. .. .. ..)
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
                          (.. .. .. .. .. .. .. ..)
                          (.. .. .. .. .. .. .. ..)
                          (r^ .. .. .. .. .. .. ..)
                          (r_ .. .. .. .. .. .. ..)
                          (<r y> .. .. .. .. .. ..)
                          (<b y> .. .. <r b> .. ..)
                          (<b y> .. .. r^ .. .. ..)
                          (<b o> .. .. y_ .. .. ..)
                          (.. r^ .. .. <y o> .. ..)
                          (bb y_ .. <o y> .. .. ..)
                          (BB yy .. rr .. .. .. ..)
                          (RR yy .. rr .. .. .. ..)
                          (BB rr .. rr .. .. .. ..)
                          (-- -- -- -- -- -- -- --)))]
        ; Regression - don't overvalue the rightmost "broken peak"
        ; (especially because it overhangs nothing!)
        [g2 (parse-grid '((.. .. .. .. .. .. .. ..)
                          (.. .. .. .. .. .. .. ..)
                          (.. .. .. .. .. .. .. ..)
                          (.. .. .. .. .. .. .. ..)
                          (.. .. .. .. .. .. .. ..)
                          (.. .. .. .. .. .. .. ..)
                          (.. .. .. .. .. .. .. ..)
                          (.. .. .. .. .. .. .. ..)
                          (.. .. .. .. .. .. .. ..)
                          (.. .. .. .. .. .. .. ..)
                          (r^ .. .. .. .. .. .. ..)
                          (r_ .. .. .. .. .. .. ..)
                          (<r y> .. .. b^ .. .. ..)
                          (<b y> .. .. r_ .. .. ..)
                          (<b y> .. .. r^ .. .. ..)
                          (<b o> .. .. y_ .. .. ..)
                          (.. r^ .. .. <y o> .. ..)
                          (bb y_ .. <o y> .. .. ..)
                          (BB yy .. rr .. .. .. ..)
                          (RR yy .. rr .. .. .. ..)
                          (BB rr .. rr .. .. .. ..)
                          (-- -- -- -- -- -- -- --)))])
    (check g1 > g2))

  (let ([g1 (parse-grid '((.. .. .. .. .. .. .. ..)
                          (.. .. .. .. .. .. .. ..)
                          (.. .. .. .. .. .. .. ..)
                          (.. .. .. .. <b r> .. ..)
                          (.. b^ .. .. .. <r o> ..)
                          (.. r_ .. b^ .. .. y^ ..)
                          (.. <r b> y_ .. r^ y_ ..)
                          (.. .. <r y> .. r_ <o b>)
                          (YY .. <r o> BB RR YY ..)
                          (.. .. .. yy RR BB YY BB)
                          (BB rr RR YY .. YY .. ..)
                          (BB RR RR BB BB RR BB ..)
                          (.. BB BB YY YY RR BB RR)
                          (RR RR .. .. BB YY YY RR)
                          (RR RR YY RR .. YY RR YY)
                          (BB .. RR .. BB RR BB BB)
                          (BB .. RR RR YY RR BB BB)
                          (RR BB YY BB YY BB YY RR)
                          (RR YY .. YY RR YY .. ..)
                          (YY BB RR .. YY RR RR YY)
                          (YY .. BB BB RR BB .. BB)
                          (-- -- -- -- -- -- -- --)))]
        [g2 (parse-grid '((.. .. .. .. .. .. .. ..)
                          (.. .. .. .. .. .. .. ..)
                          (.. .. .. .. .. .. .. ..)
                          (.. .. .. .. .. .. .. ..)
                          (.. b^ .. .. .. <r o> ..)
                          (.. r_ .. b^ .. .. y^ r^)
                          (.. <r b> y_ .. r^ y_ b_)
                          (.. .. <r y> .. r_ <o b>)
                          (YY .. <r o> BB RR YY ..)
                          (.. .. .. yy RR BB YY BB)
                          (BB rr RR YY .. YY .. ..)
                          (BB RR RR BB BB RR BB ..)
                          (.. BB BB YY YY RR BB RR)
                          (RR RR .. .. BB YY YY RR)
                          (RR RR YY RR .. YY RR YY)
                          (BB .. RR .. BB RR BB BB)
                          (BB .. RR RR YY RR BB BB)
                          (RR BB YY BB YY BB YY RR)
                          (RR YY .. YY RR YY .. ..)
                          (YY BB RR .. YY RR RR YY)
                          (YY .. BB BB RR BB .. BB)
                          (-- -- -- -- -- -- -- --)))])
    (check g1 > g2))

  (let ([g1 (parse-grid '((.. .. .. .. .. .. .. ..)
                          (.. .. .. .. .. .. .. ..)
                          (.. .. .. .. .. .. .. ..)
                          (.. .. .. .. .. y^ .. ..)
                          (.. <y o> .. .. y_ .. ..)
                          (<r y> <b b> .. r^ .. ..)
                          (r^ .. .. <o b> o_ .. ..)
                          (b_ .. .. .. <b r> .. ..)
                          (bb .. .. .. <b r> .. ..)
                          (BB .. .. .. .. RR <y r>)
                          (.. <y b> <y o> .. <y b>)
                          (RR YY <b o> BB .. RR YY)
                          (BB .. .. y^ RR YY BB RR)
                          (.. .. .. y_ BB YY RR ..)
                          (RR BB .. YY YY BB RR BB)
                          (YY .. .. .. BB YY BB YY)
                          (YY .. .. BB RR .. .. ..)
                          (BB RR BB .. YY .. RR RR)
                          (RR BB YY RR YY BB .. ..)
                          (YY YY RR RR BB YY BB YY)
                          (RR .. BB YY YY BB .. RR)
                          (-- -- -- -- -- -- -- --)))]
        ; Prefer to play the <y r> on the right side instead of increasing the
        ; danger on the left side
        [g2 (parse-grid '((.. .. .. .. .. .. .. ..)
                          (.. .. .. .. .. .. .. ..)
                          (.. .. .. .. .. .. .. ..)
                          (y^ .. .. .. .. y^ .. ..)
                          (r_ <y o> .. .. y_ .. ..)
                          (<r y> <b b> .. r^ .. ..)
                          (r^ .. .. <o b> o_ .. ..)
                          (b_ .. .. .. <b r> .. ..)
                          (bb .. .. .. <b r> .. ..)
                          (BB .. .. .. .. RR .. ..)
                          (.. <y b> <y o> .. <y b>)
                          (RR YY <b o> BB .. RR YY)
                          (BB .. .. y^ RR YY BB RR)
                          (.. .. .. y_ BB YY RR ..)
                          (RR BB .. YY YY BB RR BB)
                          (YY .. .. .. BB YY BB YY)
                          (YY .. .. BB RR .. .. ..)
                          (BB RR BB .. YY .. RR RR)
                          (RR BB YY RR YY BB .. ..)
                          (YY YY RR RR BB YY BB YY)
                          (RR .. BB YY YY BB .. RR)
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
                          (.. y^ .. .. .. .. .. ..)
                          (.. y_ .. .. .. .. .. ..)
                          (.. y^ .. .. .. .. .. ..)
                          (.. r_ .. .. .. .. .. ..)
                          (.. r^ .. .. .. .. .. ..)
                          (.. r_ .. .. .. .. .. ..)
                          (<y y> .. .. .. .. .. ..)
                          (.. y^ .. .. .. .. .. ..)
                          (.. r_ <b r> .. .. .. ..)
                          (yy b^ .. <r y> .. .. ..)
                          (yy y_ .. rr .. .. .. ..)
                          (YY y^ b^ bb .. .. .. ..)
                          (BB y_ b_ bb .. .. oo ..)
                          (-- -- -- -- -- -- -- --)))]
        ; The only remaining fuel is on the left; playing in the middle/right
        ; is irrelevant here.
        ; The bug was that we were shattering *everything* so the AI
        ; assumed that the leftmost <y was guaranteed to fall.
        ; The fix was to only shatter stable peaks
        [g2 (parse-grid '((.. .. .. .. .. .. .. ..)
                          (.. .. .. .. .. .. .. ..)
                          (.. .. .. .. .. .. .. ..)
                          (.. .. .. .. .. .. .. ..)
                          (.. .. .. .. .. .. .. ..)
                          (.. .. .. .. .. .. .. ..)
                          (.. .. .. .. .. .. .. ..)
                          (.. .. .. .. .. .. .. ..)
                          (.. .. .. .. .. .. .. ..)
                          (.. .. .. .. .. .. .. ..)
                          (.. y^ .. .. .. .. .. ..)
                          (.. r_ .. .. .. .. .. ..)
                          (.. r^ .. .. .. .. .. ..)
                          (.. r_ .. .. .. .. .. ..)
                          (<y y> .. .. .. .. .. ..)
                          (.. y^ .. .. y^ .. .. ..)
                          (.. r_ <b r> y_ .. .. ..)
                          (yy b^ .. <r y> .. .. ..)
                          (yy y_ .. rr .. .. .. ..)
                          (YY y^ b^ bb .. .. .. ..)
                          (BB y_ b_ bb .. .. oo ..)
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
                          (.. .. .. .. .. .. .. ..)
                          (yy .. .. .. .. .. .. ..)
                          (yy .. b^ .. .. .. .. ..)
                          (<y r> b_ .. .. .. .. ..)
                          (.. <r b> .. .. .. .. ..)
                          (.. <r b> .. .. .. .. ..)
                          (YY .. BB .. .. .. .. ..)
                          (BB .. RR .. .. .. .. ..)
                          (BB .. YY .. .. .. .. ..)
                          (YY RR BB .. .. r^ .. ..)
                          (YY RR YY .. .. r_ .. ..)
                          (RR BB YY .. .. <r y> ..)
                          (RR .. .. .. .. .. yy ..)
                          (-- -- -- -- -- -- -- --)))]
        ; TRICKY - destroying the BB exposes a new problem that didn't
        ; look like a problem before... Maybe we should add a rule/heuristic
        ; that "a pillar should never depend on more than one other pillar"
        ; But then again, probably not because in 500 games this problem only
        ; happened once. I think we really need to start understanding
        ; which pillars we intend to destroy and which we intend to fall...
        [g2 (parse-grid '((.. .. .. .. .. .. .. ..)
                          (.. .. .. .. .. .. .. ..)
                          (.. .. .. .. .. .. .. ..)
                          (.. .. .. .. .. .. .. ..)
                          (.. .. .. .. .. .. .. ..)
                          (.. .. .. .. .. .. .. ..)
                          (.. .. .. .. .. .. .. ..)
                          (.. .. .. .. .. .. .. ..)
                          (.. .. .. .. .. .. .. ..)
                          (yy .. .. .. .. .. .. ..)
                          (yy .. .. .. .. .. .. ..)
                          (<y r> .. .. .. .. .. ..)
                          (.. <r b> .. .. .. .. ..)
                          (.. <r b> .. .. .. .. ..)
                          (YY .. BB .. .. .. .. ..)
                          (BB .. RR .. .. .. .. ..)
                          (BB .. YY .. .. .. .. ..)
                          (YY RR BB .. .. r^ .. ..)
                          (YY RR YY .. .. r_ .. ..)
                          (RR BB YY .. .. <r y> b^)
                          (RR .. .. .. .. .. yy b_)
                          (-- -- -- -- -- -- -- --)))])
    (check g1 > g2))

  (let ([g1 (parse-grid '((.. .. .. .. .. .. .. ..)
                          (.. .. .. .. .. .. .. ..)
                          (.. .. .. .. .. .. .. ..)
                          (.. .. .. .. .. .. .. ..)
                          (.. .. .. .. .. .. .. ..)
                          (.. .. .. .. .. .. .. ..)
                          (.. .. .. .. .. .. .. ..)
                          (.. .. .. .. b^ .. .. ..)
                          (.. .. .. .. b_ .. <y b>)
                          (.. .. .. .. BB .. .. BB)
                          (.. .. .. .. .. RR .. ..)
                          (YY .. .. .. .. YY .. ..)
                          (.. .. .. .. .. .. YY ..)
                          (.. .. oo .. BB .. .. ..)
                          (-- -- -- -- -- -- -- --)))]
        ; Regression: the shatter operation was naive, and the grid below played
        ; the <b b> thinking it would get a horizontal combo with the two BBs.
        ; Obviously this is not correct because the rightmost BB would be gone.
        ; This is why we now use alternate colors for shattered pillars.
        ; In the below example, the rightmost blues would be assigned the color
        ; 'b2 instead of 'b after being shattered.
        ; (Should the shatter operation simply destroy them instead?)
        [g2 (parse-grid '((.. .. .. .. .. .. .. ..)
                          (.. .. .. .. .. .. .. ..)
                          (.. .. .. .. .. .. .. ..)
                          (.. .. .. .. .. .. .. ..)
                          (.. .. .. .. .. .. .. ..)
                          (.. .. .. .. .. .. .. ..)
                          (.. .. .. .. .. .. .. ..)
                          (.. .. .. .. .. <b b> ..)
                          (.. .. .. .. .. .. <y b>)
                          (.. .. .. .. BB .. .. BB)
                          (.. .. .. .. .. RR .. ..)
                          (YY .. .. .. .. YY .. ..)
                          (.. .. .. .. .. .. YY ..)
                          (.. .. oo .. BB .. .. ..)
                          (-- -- -- -- -- -- -- --)))])
    (check g1 > g2))

  (let ([g1 (parse-grid '((.. .. .. .. .. .. .. ..)
                          (.. .. .. .. .. .. .. ..)
                          (.. .. .. .. .. .. .. b^)
                          (.. .. .. .. .. .. .. r_)
                          (.. .. .. .. .. .. .. r^)
                          (.. .. .. .. .. .. .. o_)
                          (.. .. .. .. .. .. .. r^)
                          (.. .. .. .. .. <r y> b_)
                          (.. .. .. .. .. .. <y b>)
                          (.. .. .. .. .. .. <y b>)
                          (.. .. .. .. .. .. YY ..)
                          (.. .. .. .. .. .. .. ..)
                          (.. oo .. .. .. .. .. ..)
                          (.. bb .. .. .. .. .. oo)
                          (-- -- -- -- -- -- -- --)))]
        [g2 (parse-grid '((.. .. .. .. .. .. .. ..)
                          (.. .. .. .. .. .. .. ..)
                          (.. .. .. .. .. .. .. b^)
                          (.. .. .. .. .. .. .. r_)
                          (.. .. .. .. .. .. .. r^)
                          (.. .. .. .. .. .. .. o_)
                          (.. .. .. .. .. .. .. r^)
                          (.. .. .. .. .. .. .. b_)
                          (.. .. .. .. .. .. <y b>)
                          (.. .. .. .. .. .. <y b>)
                          (.. .. .. .. .. .. YY ..)
                          (.. .. .. .. .. .. .. ..)
                          (.. oo .. .. .. .. .. ..)
                          (.. bb .. <r y> .. .. oo)
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
                          (.. .. .. .. .. .. <y b>)
                          (.. .. .. .. .. .. <y b>)
                          (.. .. .. .. .. .. YY ..)
                          (.. .. .. .. .. .. .. ..)
                          (.. .. .. .. .. .. .. ..)
                          (yy .. .. <r b> .. .. oo)
                          (-- -- -- -- -- -- -- --)))]
        [g2 (parse-grid '((.. .. .. .. .. .. .. ..)
                          (.. .. .. .. .. .. .. ..)
                          (.. .. .. .. .. .. .. ..)
                          (.. .. .. .. .. .. .. ..)
                          (.. .. .. .. .. .. .. ..)
                          (.. .. .. .. .. .. .. ..)
                          (.. .. .. .. .. .. .. r^)
                          (.. .. .. .. .. .. .. b_)
                          (.. .. .. .. .. .. <y b>)
                          (.. .. .. .. .. .. <y b>)
                          (.. .. .. .. .. .. YY ..)
                          (.. .. .. .. .. .. .. ..)
                          (.. .. .. .. .. .. .. ..)
                          (yy .. .. .. .. .. .. oo)
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
                          (.. .. .. .. .. .. .. ..)
                          (<r y> .. .. bb .. .. ..)
                          (<r y> .. .. bb <b y> BB)
                          (RR .. .. .. BB BB .. ..)
                          (YY .. YY .. .. .. YY RR)
                          (.. YY .. rr .. .. .. ..)
                          (-- -- -- -- -- -- -- --)))]
        ; Regression: a naive approach to shatter thinks we will get a yellow
        ; horizontal group in the grid below.
        ; So we consider only vertical groups after the shatter.
        [g2 (parse-grid '((.. .. .. .. .. .. .. ..)
                          (.. .. .. .. .. .. .. ..)
                          (.. .. .. .. .. .. .. ..)
                          (.. .. .. .. .. .. .. ..)
                          (.. .. .. .. .. .. .. ..)
                          (.. .. .. .. .. .. .. ..)
                          (.. .. .. .. .. .. .. ..)
                          (.. .. .. .. .. .. .. ..)
                          (.. .. .. .. .. .. .. ..)
                          (<r y> .. .. bb .. .. ..)
                          (<r y> .. .. bb .. .. BB)
                          (RR .. <b y> BB BB .. ..)
                          (YY .. YY .. .. .. YY RR)
                          (.. YY .. rr .. .. .. ..)
                          (-- -- -- -- -- -- -- --)))])
    (check g1 > g2))

  (let ([g1 (parse-grid '((.. .. .. .. .. .. .. ..)
                          (.. .. .. .. .. .. .. ..)
                          (.. .. .. .. .. .. .. ..)
                          (.. .. .. .. .. .. .. ..)
                          (.. .. .. .. .. .. .. ..)
                          (.. .. .. .. .. .. .. ..)
                          (.. .. .. .. r^ .. .. ..)
                          (.. .. .. .. r_ .. .. ..)
                          (.. .. .. .. <o r> .. ..)
                          (.. .. .. .. RR .. .. ..)
                          (.. .. .. .. .. .. .. ..)
                          (.. .. .. .. .. .. .. ..)
                          (.. .. .. .. YY .. .. ..)
                          (.. .. .. .. BB .. .. ..)
                          (.. .. .. .. .. <r b> ..)
                          (.. .. .. .. BB RR BB ..)
                          (.. .. .. RR YY RR BB ..)
                          (.. bb .. BB YY BB YY RR)
                          (.. bb yy .. RR YY .. ..)
                          (.. BB RR .. YY RR RR YY)
                          (.. .. BB BB RR BB .. BB)
                          (-- -- -- -- -- -- -- --)))]
        [g2 (parse-grid '((.. .. .. .. .. .. .. ..)
                          (.. .. .. .. .. .. .. ..)
                          (.. .. .. .. .. .. .. ..)
                          (.. .. .. .. .. .. .. ..)
                          (.. .. .. .. .. .. .. ..)
                          (.. .. .. .. .. .. .. ..)
                          (.. .. .. .. .. .. .. ..)
                          (.. .. .. .. .. .. .. ..)
                          (.. .. .. .. <o r> .. ..)
                          (.. .. .. .. RR .. .. ..)
                          (.. .. .. .. .. .. .. ..)
                          (.. .. .. .. .. .. .. ..)
                          (.. .. .. .. YY .. .. ..)
                          (.. .. .. .. BB .. .. ..)
                          (.. .. .. r^ .. <r b> ..)
                          (.. .. .. r_ BB RR BB ..)
                          (.. .. .. RR YY RR BB ..)
                          (.. bb .. BB YY BB YY RR)
                          (.. bb yy .. RR YY .. ..)
                          (.. BB RR .. YY RR RR YY)
                          (.. .. BB BB RR BB .. BB)
                          (-- -- -- -- -- -- -- --)))])
    (check g1 > g2))

  (let ([g1 (parse-grid '((.. .. .. .. .. .. .. ..)
                          (.. .. .. .. .. .. .. ..)
                          (.. .. .. .. .. .. .. ..)
                          (.. .. .. .. .. .. .. ..)
                          (.. .. .. .. .. .. .. ..)
                          (.. .. .. .. .. .. .. ..)
                          (.. .. b^ .. .. .. .. ..)
                          (<b r> b_ .. .. .. .. ..)
                          (.. <r o> .. .. .. .. BB)
                          (.. .. bb .. .. bb .. YY)
                          (.. .. BB .. .. BB YY ..)
                          (.. .. .. BB .. RR BB BB)
                          (.. .. .. BB BB RR BB YY)
                          (BB rr YY YY .. .. RR YY)
                          (YY RR BB .. RR RR BB ..)
                          (BB YY YY BB BB YY RR BB)
                          (YY .. .. BB RR RR BB ..)
                          (YY RR BB RR RR BB RR YY)
                          (RR .. YY RR BB YY BB YY)
                          (YY RR .. YY YY BB RR RR)
                          (YY .. YY .. .. RR RR YY)
                          (-- -- -- -- -- -- -- --)))]
        [g2 (parse-grid '((.. .. .. .. .. .. .. ..)
                          (.. .. .. .. .. .. .. ..)
                          (.. .. .. .. .. .. .. ..)
                          (.. .. .. .. .. .. .. ..)
                          (.. .. .. .. .. .. .. ..)
                          (.. .. .. .. .. .. .. ..)
                          (.. .. .. .. .. .. .. ..)
                          (<b r> .. .. .. b^ .. ..)
                          (.. <r o> .. .. b_ .. BB)
                          (.. .. bb .. .. bb .. YY)
                          (.. .. BB .. .. BB YY ..)
                          (.. .. .. BB .. RR BB BB)
                          (.. .. .. BB BB RR BB YY)
                          (BB rr YY YY .. .. RR YY)
                          (YY RR BB .. RR RR BB ..)
                          (BB YY YY BB BB YY RR BB)
                          (YY .. .. BB RR RR BB ..)
                          (YY RR BB RR RR BB RR YY)
                          (RR .. YY RR BB YY BB YY)
                          (YY RR .. YY YY BB RR RR)
                          (YY .. YY .. .. RR RR YY)
                          (-- -- -- -- -- -- -- --)))])
    (check g1 > g2))

  (let ([g1 (parse-grid '((.. .. .. .. .. .. .. ..)
                          (.. .. .. .. .. .. .. ..)
                          (.. .. .. .. .. .. .. ..)
                          (.. .. .. .. .. .. .. ..)
                          (.. .. .. .. .. .. .. ..)
                          (.. .. .. .. .. .. .. ..)
                          (.. .. .. .. .. .. .. ..)
                          (.. .. .. .. <r y> <b r>)
                          (.. oo yy .. .. yy BB RR)
                          (BB BB YY .. .. YY RR BB)
                          (YY RR BB YY .. RR YY BB)
                          (YY YY RR BB BB YY BB RR)
                          (RR YY BB RR BB .. BB RR)
                          (.. .. BB YY RR .. RR BB)
                          (.. YY YY BB .. YY YY BB)
                          (YY RR BB RR YY YY .. RR)
                          (RR BB .. .. YY .. YY ..)
                          (.. .. RR BB RR RR YY RR)
                          (RR RR YY YY RR BB .. BB)
                          (BB BB .. RR .. BB YY RR)
                          (.. YY .. YY YY RR BB YY)
                          (-- -- -- -- -- -- -- --)))]
        ; Regression: The oo in the 2nd column was burst for no reason.
        [g2 (parse-grid '((.. .. .. .. .. .. .. ..)
                          (.. .. .. .. .. .. .. ..)
                          (.. .. .. .. .. .. .. ..)
                          (.. .. .. .. .. .. .. ..)
                          (.. .. .. .. .. .. .. ..)
                          (.. .. .. .. .. .. .. ..)
                          (.. .. .. .. .. .. .. ..)
                          (.. .. .. .. <r y> <b r>)
                          (.. .. yy .. .. yy BB RR)
                          (BB BB YY .. .. YY RR BB)
                          (YY RR BB YY .. RR YY BB)
                          (YY YY RR BB BB YY BB RR)
                          (RR YY BB RR BB .. BB RR)
                          (.. .. BB YY RR .. RR BB)
                          (.. YY YY BB .. YY YY BB)
                          (YY RR BB RR YY YY .. RR)
                          (RR BB .. .. YY .. YY ..)
                          (.. .. RR BB RR RR YY RR)
                          (RR RR YY YY RR BB .. BB)
                          (BB BB .. RR .. BB YY RR)
                          (.. YY .. YY YY RR BB YY)
                          (-- -- -- -- -- -- -- --)))])
    (check g1 > g2))
  ) ; end test submodule
