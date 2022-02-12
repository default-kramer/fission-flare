#lang typed/racket

(provide grid-modifier GridModifier grid-fall grid-burst grid-destroy
         grid-locs grid-count in-bounds? grid-clear
         parse-grid print-grid parse-occupant parse-queue
         )

(require "data.rkt"
         "../typed-utils.rkt"
         )

(module+ test (require typed/rackunit))

(: in-bounds? (-> Grid Loc Boolean))
(define (in-bounds? grid loc)
  (let ([x (loc-x loc)]
        [y (loc-y loc)])
    (and (x . >= . 0)
         (y . >= . 0)
         (x . < . (grid-width grid))
         (y . < . (grid-height grid))
         #t)))

; Allows a series of modifications to a grid in an imperative style. For example:
#;(let ([mod! (grid-modifier grid)])
    (mod! 'remove loc)
    (mod! 'set loc new-occ)
    (mod! 'build))
; We could use `with-controller-type` here for a more consistent codebase,
; but this code is already working and is never used from untyped code.
(define-type GridModifier (case-> (-> 'set Loc Occupant Void)
                                  (-> 'remove Loc Void)
                                  (-> 'build (U #f Grid))))
(: grid-modifier (-> Grid GridModifier))
(define (grid-modifier [g : Grid])
  (let* ([ht : GridHT (grid-ht g)]
         [width : Dimension (grid-width g)]
         [height : Dimension (grid-height g)]
         [any-removed? : Boolean #f]
         [ok? : Boolean #t])
    (define (in-bounds? [loc : Loc])
      (let ([x (loc-x loc)]
            [y (loc-y loc)])
        (and (x . >= . 0)
             (y . >= . 0)
             (x . < . width)
             (y . < . height))))
    (case-lambda
      [([id : 'remove] [loc : Loc])
       (when ok?
         (if (in-bounds? loc)
             (begin
               (set! any-removed? #t)
               (set! ht (hash-remove ht loc)))
             (set! ok? #f)))]
      [([id : 'set] [loc : Loc] [occ : Occupant])
       (when ok?
         (if (and (in-bounds? loc)
                  (not (hash-ref ht loc #f)))
             (set! ht (hash-set ht loc occ))
             (set! ok? #f)))]
      [([id : 'build])
       (and ok?
            (let ([grid (make-grid ht width height)])
              (if any-removed?
                  (grid-after-destroy grid)
                  grid)))])))

; For immutability, occupants will not know their location on the grid.
; This struct is used to tell them their own location, as well as the entire grid state.
(struct occ-context ([loc : Loc]
                     [grid : Grid]) #:transparent)

; Returns the list of locs that need to be unblocked in order for
; this occupant to fall. A loc is unblocked if it is possible for something
; to fall into it. Note that "unblocked" and "vacant" are different things.
; A vacant loc is always unblocked.
; An occupied loc is unblocked if its occupant is going to fall.
(: fall-deps (-> Occupant occ-context (U 'blocked (Listof Loc))))
(define (fall-deps occ ctx)
  (define loc (occ-context-loc ctx))
  (define below (loc-neighbor loc 'd))
  (cond
    [(catalyst? occ)
     (let ([dir (catalyst-direction occ)])
       (if dir
           (list below (loc-neighbor loc dir))
           (list below)))]
    [(fuel? occ)
     (if (fuel-falling? occ)
         (list below)
         'blocked)]
    [(contaminant? occ)
     (list below)]
    [(ground? occ)
     'blocked]))

; Returns an occupant to replace the current one, or #f indicating "no change".
; This allows the joined catalyst to replace itself with a half catalyst
; when it detects that its partner has been destroyed
(: after-destroy (-> Occupant occ-context (U #f Occupant)))
(define (after-destroy occ ctx)
  (cond
    [(and (catalyst? occ)
          (catalyst-direction occ))
     (let* ([my-dir (catalyst-direction occ)]
            [my-loc (occ-context-loc ctx)]
            [grid (occ-context-grid ctx)]
            [partner-loc (loc-neighbor my-loc my-dir)]
            [partner (grid-get grid partner-loc)])
       (if partner
           #f ; partner is still there, no change needed
           (struct-copy catalyst occ [direction #f])))]
    [else #f]))

(: grid-fall (-> Grid (U #f Grid)))
(define (grid-fall g)
  (define height (grid-height g))
  (define width (grid-width g))
  (define ht (grid-ht g))

  ; Create a mutable vector that can hold a value for every loc in the grid.
  ; We start with 'unsure but when we are done every value should be
  ; either #t meaning "blocked" or #f meaning "unblocked".
  (define-type BlockedFlag (U #t #f 'unsure))
  (define vec : (Vectorof BlockedFlag)
    (make-vector (* height width) 'unsure))
  (define (vecset! [x : Integer] [y : Integer] [val : BlockedFlag])
    (vector-set! vec (+ x (* y width)) val))
  (define (vecget [x : Integer] [y : Integer])
    (vector-ref vec (+ x (* y width))))

  ; Calculate whether (x,y) is blocked or unblocked.
  ; Also mutates the vector once we are sure we know the answer.
  (: calc-blocked (-> Integer Integer (Listof Loc) Boolean))
  (define (calc-blocked x y assume-unblocked)
    (define orig-x : Integer x)
    (define orig-y : Integer y)
    (define (maybe-set! [x : Integer] [y : Integer] [blocked? : BlockedFlag])
      ; If we discovered that (x,y) is unblocked, we don't know that for sure unless
      ; our list of assumptions is empty. However, if we discovered that (x,y) is
      ; blocked, we know that is correct -- the assumptions can only unblock, never block.
      (when (or blocked?
                (empty? assume-unblocked))
        (vecset! x y blocked?))
      blocked?)

    (: blocked? (-> Integer Integer Boolean))
    (define (blocked? [x : Integer] [y : Integer])
      (cond
        ; It can be neat to uncomment the following condition and play with no
        ; ground, but I haven't tested that in forever.
        ;[(< y 0) #f] ; allow occupants to fall into oblivion
        [(or (< y 0)
             (< x 0)
             (>= x width)
             (>= y height))
         #t]
        [(member (cons x y) assume-unblocked)
         #f]
        [else
         (let ([blocked-flag (vecget x y)])
           (case blocked-flag
             [(unsure)
              ; Right now, (x,y) refers to a dependency.
              ; We are not sure whether that dependency is blocked or not, so we recurse,
              ; adding the original loc to the `assume-unblocked` list to break
              ; any circular dependencies.
              ; ("Assume that I am unblocked, and check my dependencies.")
              (calc-blocked x y (cons (make-loc orig-x orig-y) assume-unblocked))]
             [else blocked-flag]))]))

    (define occ (hash-ref ht (cons x y) #f))
    (if (not occ)
        (maybe-set! x y #f)
        (let* ([state (occ-context (make-loc x y) g)]
               [fall-deps (fall-deps occ state)])
          (case fall-deps
            [(blocked)
             (maybe-set! x y #t)]
            [else
             (maybe-set! x y (ormap (lambda ([loc : Loc])
                                      (blocked? (loc-x loc) (loc-y loc)))
                                    fall-deps))]))))

  ; This pass calculates who is blocked and unblocked
  (for ([y (in-range 0 height)])
    (for ([x (in-range 0 width)])
      (calc-blocked x y '())))

  ; This pass moves every unblocked occupant down.
  (define new-ht : GridHT ht)
  (for ([y (in-range 0 height)])
    (for ([x (in-range 0 width)])
      (let ([blocked? (vecget x y)]
            [occ (hash-ref ht (cons x y) #f)])
        (when (and occ (not blocked?))
          (set! new-ht (hash-remove new-ht (make-loc x y)))
          (when (>= y 0)
            (set! new-ht (hash-set new-ht (make-loc x (sub1 y)) occ)))))))

  ; Return #f if none fell
  (define any-fell? (not (eq? new-ht ht)))
  (and any-fell?
       (struct-copy grid g [ht new-ht])))


; Destroy all blank occupants
(: grid-burst (-> Grid (U #f Grid)))
(define (grid-burst grid)
  (define mod! (grid-modifier grid))
  (define any? : Boolean #f)
  (for ([loc (grid-locs grid)])
    (let* ([occ (grid-get grid loc)])
      (when (can-burst? occ)
        (set! any? #t)
        (mod! 'remove loc))))
  (and any?
       (or (mod! 'build)
           (fail "grid-burst failed"))))

(: grid-after-destroy (-> Grid Grid))
(define (grid-after-destroy grid)
  (define mod! (grid-modifier grid))
  (for ([loc (grid-locs grid)])
    (let* ([occ (grid-get grid loc)]
           [new-occ (and occ
                         (after-destroy occ (occ-context loc grid)))])
      (when new-occ
        (mod! 'remove loc)
        (mod! 'set loc new-occ))))
  (or (mod! 'build)
      (fail "grid-after-destroy failed")))

(define-type Mode (U 'vertical 'horizontal 'both))

; Finds rows and columns of adjacent occupants sharing the same color.
; A group-count of 4 means the group must contain 4 or more occupants.
(: find-destruction-groups (-> Grid Integer Mode (Listof DestructionGroup)))
(define (find-destruction-groups grid group-count mode)
  (define width (grid-width grid))
  (define height (grid-height grid))

  ; A mutable variable holding the list of groups to return:
  (define groups : (Listof DestructionGroup) '())

  ; The Grouping type is used internally while we are finding groups
  (define-type Grouping (Pairof Color (Listof (Pairof Loc Occupant))))

  ; A contaminated group gets +1 to its horizontal length and -1 to its
  ; vertical length. This makes it easier to destroy horizontally.
  (define (effective-length [group : Grouping])
    (let ([contaminated? (ormap contaminant? (flatten group))]
          [horizontal? (match (cdr group)
                         [(list (cons loc-a occ-a)
                                (cons loc-b occ-b)
                                more ...)
                          (= (loc-y loc-a) (loc-y loc-b))]
                         [else #f])]
          [raw-length (length (cdr group))])
      (cond
        [(not contaminated?) raw-length]
        [horizontal? (add1 raw-length)]
        [else (sub1 raw-length)])))

  ; This procedure can search rows or columns
  (: find-groups! (-> Integer Integer (-> Integer Integer Loc) Void))
  (define (find-groups! imax jmax ij->loc)
    (for ([i (in-range 0 imax)])
      (define current-group : Grouping
        (list #f))
      (define (finish-group!)
        (when (and (car current-group)
                   (>= (effective-length current-group) group-count))
          (let* ([key : Color (car current-group)]
                 [items : (Listof (Pairof Loc Occupant)) (cdr current-group)]
                 [items : (Listof Destruction)
                        (for/list ([pair items]
                                   [i (in-range 999)])
                          (make-destruction (car pair) (cdr pair) i))]
                 [dg (make-destruction-group key items)])
            (set! groups (cons dg groups))))
        (set! current-group (list #f)))
      (define (start-new-group! [key : Color] [loc : Loc] [occ : Occupant])
        (finish-group!)
        (set! current-group (list key (cons loc occ))))
      (define (continue-group! [loc : Loc] [occ : Occupant])
        (let ([key (car current-group)]
              [items (cdr current-group)])
          (set! current-group (ann (cons key (cons (cons loc occ) items))
                                   Grouping))))

      (for ([j (in-range 0 jmax)])
        (let* ([loc (ij->loc i j)]
               [occ (grid-get grid loc)]
               [key (and occ (occupant-color occ))])
          (cond
            [(not key)
             (finish-group!)]
            [(equal? key (car current-group))
             (continue-group! loc occ)]
            [occ
             (start-new-group! key loc occ)]
            [else
             (fail "find-groups! failed")])))
      ; We've reached the end of the row or column:
      (finish-group!)))

  (when (member mode '(vertical both))
    (find-groups! width height make-loc))
  (when (member mode '(horizontal both))
    (find-groups! height width (lambda (i j) (make-loc j i))))
  groups)

(: grid-destroy (->* (Grid) (Integer #:mode Mode)
                     (values (U #f Grid) (Listof DestructionGroup))))
(define (grid-destroy grid [group-count 4] #:mode [mode 'both])
  (define groups : (Listof DestructionGroup)
    (find-destruction-groups grid group-count mode))
  (if (empty? groups)
      (values #f groups)
      (let ([mod! (grid-modifier grid)])
        (for ([group groups])
          (for ([loc (destruction-group-locs group)])
            (mod! 'remove loc)))
        (let* ([grid (or (mod! 'build)
                         (fail "grid-destroy failed"))])
          (values grid groups)))))

(: grid-clear (-> Grid (-> (U #f Occupant) Any) Grid))
(define (grid-clear grid predicate)
  (let ([mod! (grid-modifier grid)])
    (for ([loc (grid-locs grid)])
      (let ([occ (grid-get grid loc)])
        (when (predicate occ)
          (mod! 'remove loc))))
    (or (mod! 'build)
        (fail "grid-clear failed"))))

(define-type IterationKey (List Dimension Dimension Direction Direction))

(: grid-iterate (-> Grid Direction Direction (Listof Loc)))
(define grid-iterate
  (let ([cache : (Listof (Pairof IterationKey (Listof Loc))) '()])

    (: do-iterate (-> Integer Integer Direction Direction (Listof Loc)))
    (define (do-iterate width height a b)
      (define ij->loc
        (case a
          [(d u) make-loc]
          [(l r) (lambda ([i : Integer] [j : Integer])
                   (make-loc j i))]
          [else "fail incomplete case" a]))
      (define (get-range dir)
        (case dir
          [(d) (in-range 0 height)]
          [(u) (in-range (sub1 height) -1 -1)]
          [(l) (in-range 0 width)]
          [(r) (in-range (sub1 width) -1 -1)]
          [else (fail "incomplete case" dir)]))
      (for*/fold ([accum : (Listof Loc) '()])
                 ([i (get-range b)]
                  [j (get-range a)])
        (cons (ij->loc i j) accum)))

    (lambda (grid a b)
      (let* ([width (grid-width grid)]
             [height (grid-height grid)]
             [key (list width height a b)]
             [found (assoc key cache)]
             #:break (when found
                       (cdr found))
             [result (do-iterate width height a b)]
             [cache-item (ann (cons key result)
                              (Pairof IterationKey (Listof Loc)))])
        (set! cache (cons cache-item cache))
        result))))

(define (grid-locs [grid : Grid])
  (grid-iterate grid 'r 'u))

(module+ test
  (define-syntax-rule (test-iterations [a b expected ...] ...)
    (begin
      (check-equal? (grid-iterate (make-empty-grid 2 3) 'a 'b)
                    '(expected ...))
      ...))
  (test-iterations [r u
                      (0 . 0) (1 . 0)
                      (0 . 1) (1 . 1)
                      (0 . 2) (1 . 2)]
                   [r d
                      (0 . 2) (1 . 2)
                      (0 . 1) (1 . 1)
                      (0 . 0) (1 . 0)]
                   [l u
                      (1 . 0) (0 . 0)
                      (1 . 1) (0 . 1)
                      (1 . 2) (0 . 2)]
                   [l d
                      (1 . 2) (0 . 2)
                      (1 . 1) (0 . 1)
                      (1 . 0) (0 . 0)]
                   [u r
                      (0 . 0) (0 . 1) (0 . 2)
                      (1 . 0) (1 . 1) (1 . 2)]
                   [u l
                      (1 . 0) (1 . 1) (1 . 2)
                      (0 . 0) (0 . 1) (0 . 2)]
                   [d r
                      (0 . 2) (0 . 1) (0 . 0)
                      (1 . 2) (1 . 1) (1 . 0)]
                   [d l
                      (1 . 2) (1 . 1) (1 . 0)
                      (0 . 2) (0 . 1) (0 . 0)]))

; Returns the count of occupants for which the predicate is true
(: grid-count (-> Grid (-> (U #f Occupant) Any) Integer))
(define (grid-count grid predicate)
  (for/fold ([count 0])
            ([loc (grid-locs grid)])
    (if (predicate (grid-get grid loc))
        (add1 count)
        count)))


; A parser is a dictionary of Symbol -> Occupant.
; A printer is the inverse dictionary, Occupant -> Symbol
; These are used to convert a grid to/from a textual representation.
(define parsers : (Listof (Pairof Symbol Occupant)) '())
(define printers : (Listof (Pairof Occupant Symbol)) '())

(define (add-parser [pattern : (U String Symbol)] [val : Occupant])
  (let ([sym : Symbol (if (string? pattern)
                          (string->symbol pattern)
                          pattern)])
    (set! parsers (cons (cons sym val) parsers))
    (set! printers (cons (cons val sym) printers))))

(define-syntax-rule (add-parsers [pattern val] ...)
  (begin
    (add-parser pattern val)
    ...))

(for ([color : Color '(r y b #f)]
      [sym : Symbol  '(r y b o)])
  (add-parsers
   [(format "<~a" sym) (make-catalyst color 'r)]
   [(format "~a>" sym) (make-catalyst color 'l)]
   [(format "~a^" sym) (make-catalyst color 'd)]
   [(format "~a_" sym) (make-catalyst color 'u)]
   [(format "~a~a" sym sym) (make-catalyst color #f)]))

(for ([color : Color '(r y b)]
      [sym : Symbol '(RR YY BB)])
  (add-parser sym (make-fuel color #f)))

(add-parser '-- (ground))

(: parse-occupant (-> Symbol (U #f Occupant)))
(define (parse-occupant sym)
  (case sym
    [(..) #f]
    [else (let ([result : (U #f (Pairof Symbol Occupant))
                        (assoc sym parsers)])
            (if result
                (cdr result)
                (fail "cannot parse:" sym)))]))

(: parse-queue (-> (Listof Symbol) (Listof (Pairof Catalyst Catalyst))))
(define (parse-queue queue)
  (match queue
    [(list) (list)]
    [(list a b rest ...)
     (let ([left (parse-occupant a)]
           [right (parse-occupant b)])
       (when (or (not (catalyst? left))
                 (not (catalyst? right)))
         (fail "Invalid queue - contains non-catalyst:" left right))
       (cons (cons left right) (parse-queue rest)))]
    [(list a)
     (fail "Invalid queue - odd number of occupants")]))

(: parse-grid (-> (Listof (Listof Symbol)) Grid))
(define (parse-grid rows)
  (define width (cast (length (first rows)) Dimension))
  (define height (cast (length rows) Dimension))
  (define grid (make-empty-grid width height))
  (define mod! (grid-modifier grid))
  (for ([y (in-range (sub1 height) -1 -1)]
        [row rows])
    (for ([x (in-range width)]
          [cell row])
      (let ([occ (parse-occupant cell)])
        (when occ
          (mod! 'set (make-loc x y) occ)))))
  (or (mod! 'build)
      (fail "parse-grid failed")))

(: print-grid (-> Grid (Listof (Listof Symbol))))
(define (print-grid grid)
  (let* ([width (grid-width grid)]
         [height (grid-height grid)])
    (for/list : (Listof (Listof Symbol))
      ([y (in-range (sub1 height) -1 -1)])
      (for/list : (Listof Symbol)
        ([x (in-range width)])
        (let ([occ (grid-get grid (make-loc x y))])
          (if occ
              (let ([result (assoc occ printers)])
                (if result
                    (cdr result)
                    (fail "lookup failed:" occ)))
              '..))))))

(module+ test
  (let* ([pattern '([.. .. YY ..]
                    [<r b> .. ..])]
         [grid (parse-grid pattern)]
         [p2 (print-grid grid)])
    (check-equal? pattern p2)))


(: grid-vertical-divorce (-> Grid Grid))
; If we rotate a <y r> by 90 degrees in either direction and place it on a grid,
; we can divorce the two halves and it will make no difference as the
; game continues. For example, the following
#;([.. y^ ..]
   [.. r_ ..]
   [anything])
; is completely equivalent to the divorced version
#;([.. yy ..]
   [.. rr ..]
   [anything])
; because if the rr drops one cell, the yy will also drop one cell.
; And if the yy is destroyed, the rr is what would be left over anyway.
; And if both are destroyed, it obviously doesn't matter anymore.
; This is still true with blanks.
; Normalizing to the divorced version could make analysis easier, because we
; have to handle singles like yy and rr no matter what.
; Also, typing y^ is harder than typing yy.
(define (grid-vertical-divorce g)
  (error "not implemented"))
