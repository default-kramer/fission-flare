#lang typed/racket

(provide make-initial-state make-empty-state state-get state-width state-height
         state-apply state-next-spawns state-waiting-frame)

(require "data.rkt"
         "grid.rkt"
         "../typed-utils.rkt"
         (prefix-in tr: (only-in typed/racket random shuffle)))

; The top 2 rows are reserved for the mover
(define mover-reserved-rows 2)

; Make sure we are explicit with our PRNGs
(: random (-> Integer Pseudo-Random-Generator Integer))
(define random tr:random)

(: shuffle (All (a) (-> (Listof a) Pseudo-Random-Generator (Listof a))))
(define (shuffle lst prng)
  (parameterize ([current-pseudo-random-generator prng])
    (tr:shuffle lst)))

(define event-collector : (Parameterof (U #f (Listof Event)))
  (make-parameter #f))

(define (add-event! [e : Event])
  (let ([events (event-collector)])
    ; We could just silently discard the event too, but right now I never intend
    ; to do that so crash instead.
    (assert events)
    (event-collector (cons e events))))

(: make-empty-state (->* (Dimension Dimension) (GameSettings) State))
(define (make-empty-state width height [settings default-game-settings])
  (let* ([given-seed (game-settings-misc:random-seed settings)]
         [prng (or (and given-seed
                        (vector->pseudo-random-generator given-seed))
                   (make-pseudo-random-generator))]
         [prng (vector->prng
                (prng->vector prng))]
         [rand-vec (prng->vector prng)]
         [wave-count (game-settings-layout:num-waves settings)])
    (make-state (make-empty-grid width height)
                #f ; mover
                (make-spawn-state (list) rand-vec)
                (make-penalty-state (game-settings-penalty:resistance settings))
                (game-settings-energy:initial settings)
                (make-layout-state wave-count rand-vec)
                #f ; current-combo
                #f ; previous-combo
                settings
                initial-stats
                #f ; game-over?
                )))

; For preview only. Does not write the PRNG back to the state (which should
; be obvious from the return value).
(: state-next-spawns (-> State Integer (Listof (Pairof Catalyst Catalyst))))
(define (state-next-spawns state count)
  (: go (-> State Integer (Listof (Pairof Catalyst Catalyst)) (Listof (Pairof Catalyst Catalyst))))
  (define (go state count accum)
    (if (= 0 count)
        (reverse accum)
        (let-values ([(state left right)
                      (state-create-spawn state)])
          (go state (sub1 count) (cons (cons left right) accum)))))
  (go state count '()))

; The color distribution for spawned catalysts is modeled as a stack of cards.
; When the stack is empty, we simply reshuffle it and continue.
(: make-unshuffled-stack (-> (Listof Color) (Listof Color) (Listof (Pairof Color Color))))
(define (make-unshuffled-stack colors-a colors-b)
  (define (list->pair [x : (List Color Color)])
    (match x [(list a b) (cons a b)]))
  (map list->pair (cartesian-product colors-a colors-b)))

(define stack-9 (make-unshuffled-stack '(r y b) '(r y b)))
(define stack-12 (make-unshuffled-stack '(r y b) '(r y b #f)))
(define stack-18 (append stack-9 stack-9))
(define stack-24 (append stack-12 stack-12))

(: state-create-spawn (-> State (Values State Catalyst Catalyst)))
(define (state-create-spawn state)
  (let* ([ss (state-spawn-state state)]
         [stack (spawn-state-card-stack ss)]
         #:break (when (not (empty? stack))
                   (let* ([color-pair (car stack)]
                          [ss (struct-copy SpawnState ss
                                           [card-stack (cdr stack)])]
                          [state (struct-copy State state
                                              [spawn-state ss])])
                     (values state
                             (make-catalyst (car color-pair) 'r)
                             (make-catalyst (cdr color-pair) 'l))))
         [settings (state-settings state)]
         [rand-vec (spawn-state-rand-vec ss)]
         [prng (vector->prng rand-vec)]
         [stack (case (game-settings-misc:catalyst-deck settings)
                  [(noblank-9) stack-9]
                  [(noblank-18) stack-18]
                  [(blank-12) stack-12]
                  [(blank-24) stack-24])]
         [stack (shuffle stack prng)]
         [rand-vec (prng->vector prng)]
         [ss (struct-copy SpawnState ss
                          [card-stack stack]
                          [rand-vec rand-vec])])
    (state-create-spawn (struct-copy State state
                                     [spawn-state ss]))))

(: replace-mover (-> State (U #f Mover) State))
(define (replace-mover state new-mover)
  (struct-copy State state [mover new-mover]))

(: state-rotate (-> State Any (U #f State)))
(define (state-rotate s clockwise?)
  (let* ([mover (state-mover s)]
         #:break (when (not mover)
                   #f)
         [grid (state-grid s)]
         [loc-a (mover-loc-a mover)]
         [loc-b (mover-loc-b mover)]
         [occ-a (mover-occ-a mover)]
         [occ-b (mover-occ-b mover)]
         [ax (loc-x loc-a)]
         [ay (loc-y loc-a)]
         [bx (loc-x loc-b)]
         [by (loc-y loc-b)]
         [dir-b (catalyst-direction occ-b)]
         #:break (when (not dir-b)
                   (fail "mover should always have a partner" mover))
         ; The values ending with a hat^ are the new values
         [(ax^ ay^ bx^ by^ dir-a^ dir-b^)
          (if clockwise?
              (case dir-b
                [(l)
                 (values ax (add1 ay) ax ay 'd 'u)]
                [(u)
                 (values (add1 bx) by bx by 'l 'r)]
                [(r)
                 (values bx by bx (add1 by) 'u 'd)]
                [(d)
                 (values ax ay (add1 ax) ay 'r 'l)])
              (case dir-b
                [(l)
                 (values ax ay ax (add1 ay) 'u 'd)]
                [(u)
                 (values bx by (add1 bx) by 'r 'l)]
                [(r)
                 (values bx (add1 by) bx by 'd 'u)]
                [(d)
                 (values (add1 ax) ay ax ay 'l 'r)]))]
         ; Prepare new occupants (assume we are going to succeed)
         [occ-a^ (change-dir occ-a dir-a^)]
         [occ-b^ (change-dir occ-b dir-b^)])

    ; Test whether the necessary squares are empty.
    ; If they are, return a new mover, otherwise return false:
    (define (try-build-mover [dx : Integer])
      (define loc-a^ (make-loc (+ dx ax^) ay^))
      (define loc-b^ (make-loc (+ dx bx^) by^))
      (define mover (make-mover occ-a^ loc-a^ occ-b^ loc-b^))
      (and (place-mover grid mover)
           mover))
    (define new-mover (or (try-build-mover 0)
                          ; wall kick:
                          (try-build-mover -1)))
    (and new-mover
         (replace-mover s new-mover))))

(define (change-dir [c : Catalyst] [d : Direction])
  (struct-copy catalyst c [direction d]))

(: place-mover (-> Grid Mover (U #f Grid)))
; Checks if the given mover can be placed onto the grid.
; Returns the resulting grid or #f.
(define (place-mover grid mover)
  (let ([mod! (grid-modifier grid)])
    (mod! 'set (mover-loc-a mover) (mover-occ-a mover))
    (mod! 'set (mover-loc-b mover) (mover-occ-b mover))
    (mod! 'build)))

(: state-move (-> State Direction (U #f State)))
(define (state-move state dir)
  (let* ([mover (state-mover state)]
         #:break (when (not mover)
                   #f)
         [grid (state-grid state)]
         [occ-a (mover-occ-a mover)]
         [occ-b (mover-occ-b mover)]
         [loc-a (mover-loc-a mover)]
         [loc-b (mover-loc-b mover)]
         ; values with a hat^ are new values
         [loc-a^ (loc-neighbor loc-a dir)]
         [loc-b^ (loc-neighbor loc-b dir)]
         [mover^ (make-mover occ-a loc-a^ occ-b loc-b^)]
         [clear? (place-mover grid mover^)])
    (cond
      [clear?
       (replace-mover state mover^)]
      [(equal? 'd dir)
       ; Couldn't move down, need to finalize.
       ; The existing mover must be valid so place-mover should succeed.
       (struct-copy State state
                    [grid (or (place-mover grid mover)
                              (fail "couldn't place mover"))]
                    [mover #f])]
      [else
       ; couldn't move in any other direction, return false
       #f])))

(: state-jump (-> State Direction (U #f State)))
; Repeatedly calls state-move up to 4 times.
; Returns #t if the state changed (even if it only moved 1 cell).
(define (state-jump state dir)
  (: go (-> State (U #f State) Integer (U #f State)))
  (define (go state result count)
    (if (= 0 count)
        result
        (let ([next (state-move state dir)])
          (or (and next (go next next (sub1 count)))
              result))))
  (go state #f 4))

(: cleanup-top (-> Grid (-> Occupant Any) (U #f Grid)))
; Clears all occupants from the top 2 rows (reserved for the mover),
; or returns #f if any occupant satisfies the fail-predicate.
(define (cleanup-top grid fail-predicate)
  (let* ([failed? : Boolean #f]
         [width (grid-width grid)]
         [height (grid-height grid)]
         [mod! (grid-modifier grid)]
         [_ (for ([x (in-range width)])
              (for ([y (in-range (- height mover-reserved-rows) height)])
                (let* ([loc (make-loc x y)]
                       [occ (grid-get grid loc)])
                  (if (and occ (fail-predicate occ))
                      (set! failed? #t)
                      (mod! 'remove loc)))))]
         #:break (when failed? #f))
    (or (mod! 'build)
        (fail "cleanup-top failed"))))

(: state-plummet (-> State (U #f State)))
(define (state-plummet state)
  (: go (-> State State))
  (define (go state)
    (define result (state-move state 'd))
    (if result
        (go result)
        state))
  (let* (#:break (when (not (state-mover state))
                   #f)
         [state (go state)]
         [grid (state-grid state)]
         ; Use `identity` as the fail-predicate here, meaning that we will fail
         ; if any occupant is found in the mover's reserved area.
         [grid (cleanup-top grid identity)]
         #:break (when (not grid)
                   #f)
         [state (struct-copy State state
                             [grid grid])])
    state))

(: state-spawn (-> State (U #f State)))
(define (state-spawn state)
  (let* ([mover (state-mover state)]
         #:break (when mover #f)
         [(state left right)
          (state-create-spawn state)]
         [grid (state-grid state)]
         [x (floor (sub1 (/ (grid-width grid) 2)))]
         [y (- (grid-height grid) 2)]
         [left-loc (make-loc x y)]
         [right-loc (make-loc (add1 x) y)]
         #:break (when (or (grid-get grid left-loc)
                           (grid-get grid right-loc))
                   (struct-copy State state
                                [game-over? 'died-spawn]))
         [stats (state-stats state)]
         [settings (state-settings state)]
         [catalyst-cost (game-settings-energy:catalyst-cost settings)]
         [stats (struct-copy Stats stats
                             [spawn-count (add1 (stats-spawn-count stats))]
                             [spawn-energy (+ catalyst-cost (stats-spawn-energy stats))])]
         [state (struct-copy State state
                             [stats stats])])
    (begin
      (add-event! (catalyst-spawned left left-loc))
      (add-event! (catalyst-spawned right right-loc))
      (after-spawn
       (replace-mover state (make-mover left left-loc right right-loc))))))

(: after-spawn (-> State State))
(define (after-spawn state)
  (let* ([settings (state-settings state)]
         [energy-cost (game-settings-energy:catalyst-cost settings)]
         [penalty-cost (game-settings-penalty:catalyst-cost settings)])
    (state-reduce state #:energy energy-cost #:penalty penalty-cost)))

(define empty-combo (combo (list) 0 (list)))

(: state-destroy (-> State (U #f State)))
(define (state-destroy state)
  (let* ([(new-grid groups)
          (grid-destroy (state-grid state))]
         #:break (when (not new-grid) #f)
         [current-combo (or (state-current-combo state)
                            empty-combo)]
         [settings (state-settings state)]
         [current-combo
          (combo-add-groups
           current-combo groups
           #:require-fuel? (game-settings-misc:combos-require-fuel? settings)
           #:fuel-value (game-settings-energy:fuel-value settings)
           #:payout-table (game-settings-energy:combo-payout settings)
           #:horizontal-bonus-factor (game-settings-energy:horizontal-bonus-factor settings))])
    (add-event! (occupants-destroyed groups))
    (struct-copy State state
                 [grid new-grid]
                 [mover #f]
                 [current-combo current-combo]
                 [game-over? (if (and (= 0 (grid-count new-grid fuel?))
                                      (equal? 0 (state-remaining-wave-count state)))
                                 'win #f)])))

(: group-count (-> DestructionGroup (-> Occupant Any) Integer))
(define (group-count group predicate)
  (let* ([items (destruction-group-items group)]
         [occs (map destruction-occ items)])
    (length (filter predicate occs))))

(: combo-add-groups (-> Combo (Listof DestructionGroup)
                        #:require-fuel? Boolean
                        #:fuel-value Integer
                        #:payout-table PayoutTable
                        #:horizontal-bonus-factor Integer
                        Combo))
; Adds more groups to an existing combo and rebuilds the scores+explanations.
; Does not handle the wave completion bonus, which will be wiped out if present.
; The payout logic is as follows:
; * Only uncontaminated groups are considered.
; * Fuel pays out a fixed amount per fuel destroyed.
; * The number of uncontaminated groups pays out per the payout table.
; * A horizontal bonus is added for each horizontal group containing at least one fuel.
;   The base amount for each is (+ total-num-fuels-destroyed half-y-coord)
;   which is then scaled based on a game setting.
;   This means if you destroy 9 fuels in the combo and have 2 horizontal groups,
;   each horizontal bonus will get credit for 9 fuels.
(define (combo-add-groups combo groups
                          #:require-fuel? require-fuel?
                          #:fuel-value fuel-value
                          #:payout-table payout-table
                          #:horizontal-bonus-factor horizontal-factor
                          )
  (define-type Explanation (List String Integer String))
  (let* ([all-groups (append groups (combo-groups combo))]
         ; Contaminated groups are never considered.
         ; Groups without fuel might be considered based on a GameSetting.
         [groups (filter (lambda ([group : DestructionGroup])
                           (and (= 0 (group-count group contaminant?))
                                (or (not require-fuel?)
                                    (positive? (group-count group fuel?)))))
                         all-groups)]
         [len (length groups)]
         [energy-payout (payout-ref payout-table len)]
         ; Use mutation to gather the total number of fuels and a list
         ; of y-coordinates of horizontal groups that qualify for the bonus.
         [fuel-count : Integer 0]
         [bonus-y-coords : (Listof Integer) (list)])
    (for ([group groups])
      (let* ([group-fuel-count (group-count group fuel?)]
             [items (destruction-group-items group)]
             [locs (map destruction-loc items)]
             ; The y-coordinate if the group is horizontal, else false
             [y : (U #f Integer)
                (match locs
                  [(list (cons x1 y) (cons x2 y) more ...)
                   y]
                  [else #f])])
        (when (and y (> group-fuel-count 0))
          (set! bonus-y-coords (cons y bonus-y-coords)))
        (set! fuel-count (+ fuel-count group-fuel-count))))
    (let* ([horizontal-explanations
            (for/list : (Listof Explanation)
              ([y bonus-y-coords])
              (let ([y-score (quotient y 2)])
                (list "Horizontal Bonus" (* horizontal-factor (+ fuel-count y-score))
                      (format "~a×(~a+~a)" horizontal-factor fuel-count y-score))))]
           [explanations : (Listof Explanation)
                         (list* (list "Fuel" (* fuel-count fuel-value)
                                      (format "~a×~a" fuel-count fuel-value))
                                (list "Combo Bonus" energy-payout
                                      (format "~a groups" len))
                                horizontal-explanations)]
           [get-score : (-> Explanation Integer) second]
           [total-payout : Integer (apply + (map get-score explanations))])
      (struct-copy Combo combo
                   [groups all-groups]
                   [total-payout total-payout]
                   [explanations explanations]))))

(: state-get (-> State Loc (U #f Occupant)))
(define (state-get state loc)
  (match (state-mover state)
    [(mover occ-a loc-a occ-b loc-b)
     #:when (equal? loc loc-a)
     occ-a]
    [(mover occ-a loc-a occ-b loc-b)
     #:when (equal? loc loc-b)
     occ-b]
    [else
     (grid-get (state-grid state) loc)]))

(: state-fall (-> State (U #f State)))
(define (state-fall state)
  (let* (#:break (when (state-mover state)
                   #f)
         [new-grid (grid-fall (state-grid state))]
         #:break (when (not new-grid)
                   #f))
    (struct-copy State state
                 [grid new-grid])))

(: state-burst (-> State (U #f State)))
(define (state-burst state)
  (let ([new-grid (grid-burst (state-grid state))])
    (and new-grid
         (struct-copy State state
                      [grid new-grid]))))

(: state-tick (-> State (U #f State)))
(define (state-tick state)
  (define (pre-spawn [state : State])
    (let* ([state (replace-falling-fuels state)]
           [state (or (add-fuel state)
                      state)])
      state))
  (if (state-mover state)
      #f ; tick has no effect
      (or
       ; finish falling before re-triggering destruction
       (state-fall state)
       (state-destroy state)
       (reward-combo state)
       (apply-penalty state)
       (state-spawn (pre-spawn state)))))

(: replace-falling-fuels (-> State State))
(define (replace-falling-fuels state)
  (let* ([grid (state-grid state)]
         [mod! (grid-modifier grid)])
    (for ([loc (grid-locs grid)])
      (let ([occ (grid-get grid loc)])
        (when (and (fuel? occ)
                   (fuel-falling? occ))
          (mod! 'remove loc)
          (mod! 'set loc (struct-copy Fuel occ [falling? #f])))))
    (struct-copy State state
                 [grid (or (mod! 'build)
                           (fail "replace-falling-fuels failed"))])))

(: add-fuel (-> State (U #f State)))
; Adds more fuel to the grid if it is time to do so.
(define (add-fuel state)
  (let* ([settings (state-settings state)]
         [layout-mode (game-settings-layout:mode settings)])
    (case layout-mode
      [(standard)
       (add-fuel/standard-layout state)]
      [(wave)
       (add-fuel/wave-layout state)])))

(: add-fuel/wave-layout (-> State (U #f State)))
(define (add-fuel/wave-layout state)

  (define-type Pattern (Listof (Listof (U #f Color))))

  (: create-pattern (-> Pseudo-Random-Generator Pattern))
  (define (create-pattern prng)
    (let* ([choices '(r y b #f)]
           [pattern : Pattern
                    (for/list ([i (in-range 4)])
                      (shuffle choices prng))]
           [grid (make-empty-grid 4 4)]
           [mod! (grid-modifier grid)]
           [_ (for ([x (in-range (length pattern))]
                    [column pattern])
                (for ([y (in-range (length column))]
                      [color column])
                  (when color
                    (mod! 'set (make-loc x y) (make-fuel color #f)))))]
           [grid (or (mod! 'build)
                     (fail "error creating wave pattern"))]
           [(any-destroyed? groups)
            (grid-destroy grid 3)])
      (if any-destroyed?
          (create-pattern prng)
          (transpose pattern))))

  (: remove-all (-> Grid Integer Integer (U #f GridModifier)))
  ; Remove all occupants from the specified area of the grid.
  ; Return #f if a fuel was found, meaning we should not spawn a new wave yet.
  (define (remove-all grid x-start x-end)
    (define height (grid-height grid))
    (define is-clear? : Boolean #t)
    (define mod! (grid-modifier grid))
    (for ([x (in-range x-start x-end 1)])
      (for ([y (in-range height)])
        (let* ([loc (make-loc x y)]
               [occ (grid-get grid loc)])
          (when (fuel? occ)
            (set! is-clear? #f))
          (when (not (ground? occ))
            (mod! 'remove loc)))))
    (and is-clear? mod!))

  (: put-new-wave (-> Grid GridModifier Integer Pattern Grid))
  ; Places a new wave of fuels starting at the given x-coordinate.
  ; Assumes the area has already been cleared.
  (define (put-new-wave grid mod! x-start pattern)
    (let* ([height (grid-height grid)]
           [y-start (floor-height grid)])
      (for ([x (in-range x-start 999)]
            [column pattern])
        (for ([y (in-range y-start 999)]
              [color column])
          (when color
            (mod! 'set (make-loc x y) (make-fuel color #f)))))
      (or (mod! 'build)
          (fail "put-new-wave failed"))))

  (: try-new-wave (-> Grid Pattern Boolean (U #f Grid)))
  ; If the specified half of the grid is ready, place a new wave.
  (define (try-new-wave grid pattern left?)
    (let* ([width (grid-width grid)]
           [mid (quotient width 2)]
           [start (if left? 0 mid)]
           [end (if left? mid width)]
           [mod! (remove-all grid start end)])
      (if mod!
          (let ([start (if left? (- mid 5) (+ mid 1))])
            (put-new-wave grid mod! start pattern))
          #f)))

  ; Main body
  (let* ([ls (state-layout-state state)]
         [count (layout-state-remaining-wave-count ls)]
         #:break (when (= 0 (or count 1))
                   #f)
         [grid (state-grid state)]
         [rand-vec (layout-state-rand-vec ls)]
         [prng (vector->prng rand-vec)]
         [pattern (create-pattern prng)]
         [new-grid (or (try-new-wave grid pattern #t)
                       (try-new-wave grid pattern #f))]
         [new-ls (struct-copy LayoutState ls
                              [rand-vec (prng->vector prng)]
                              [remaining-wave-count (and count (sub1 count))])]
         [new-state (and new-grid
                         (struct-copy State state
                                      [grid new-grid]
                                      [layout-state new-ls]))])
    ; Recurse only once: if we just did the left half, we might also
    ; need to do the right half
    (and new-state
         (or (add-fuel/wave-layout new-state)
             new-state))))


(: reward-combo (-> State (U #f State)))
(define (reward-combo state)
  (define (add-wave-completion-bonus [combo : Combo] [bonus : Integer])
    (let* ([total-payout (combo-total-payout combo)]
           [total-payout (+ total-payout bonus)]
           [explanations (combo-explanations combo)]
           [explanations (append explanations
                                 (list (list "Wave Completion" bonus "")))])
      (struct-copy Combo combo
                   [total-payout total-payout]
                   [explanations explanations])))
  (let* ([combo (state-current-combo state)]
         #:break (when (not combo) #f)
         [groups (combo-groups combo)]
         [result1 (maybe-drop-floor state groups 'l)]
         [state (or result1 state)]
         [result2 (maybe-drop-floor state groups 'r)]
         [state (or result2 state)]
         [settings (state-settings state)]
         [bonus-amount (game-settings-energy:wave-completion-bonus settings)]
         [wave-completion-bonus (+ (if result1 bonus-amount 0)
                                   (if result2 bonus-amount 0))]
         [combo (if (= wave-completion-bonus 0)
                    combo
                    (add-wave-completion-bonus combo wave-completion-bonus))]
         [energy (state-energy state)]
         [energy (+ energy (combo-total-payout combo))]
         ; Use the energy combo to build an attack combo.
         ; Calculate using the same DestructionGroups but different payouts.
         [attack-combo
          (combo-add-groups
           empty-combo (combo-groups combo)
           #:require-fuel? #f
           #:fuel-value 0
           #:payout-table (game-settings-attack:combo-payout settings)
           #:horizontal-bonus-factor (game-settings-attack:horizontal-bonus-factor settings))]
         [attack-amount (combo-total-payout attack-combo)])
    (when (> attack-amount 0)
      (add-event! (make-attack-generated attack-amount)))
    (struct-copy State state
                 [current-combo #f]
                 [previous-combo combo]
                 [energy energy])))

(: floor-height (-> (U State Grid) Integer))
; Returns the first y-coordinate that is not ground.
; Assumes that ground is level. In other words, we assume that if (x, y) is
; ground then (x2, y) is also ground for any x2.
(define (floor-height x)
  (define grid : Grid (if (state? x)
                          (state-grid x)
                          x))
  (: go (-> Integer Integer))
  (define (go y)
    (if (ground? (grid-get grid (make-loc 0 y)))
        (go (add1 y))
        y))
  (go 0))

(: maybe-drop-floor (-> State (Listof DestructionGroup) (U 'l 'r) (U #f State)))
(define (maybe-drop-floor state groups side)
  (define settings (state-settings state))
  (define max-drop-amount (game-settings-penalty:floor-drop-amount settings))
  (define width (state-width state))
  (define mid (quotient width 2))
  (define x-start : Integer
    (case side
      [(l) 0]
      [(r) mid]))
  (define x-end : Integer
    (case side
      [(l) mid]
      [(r) width]))
  (define (destroyed-a-fuel?)
    (define retval : Boolean #f)
    (for ([group groups])
      (for ([item (destruction-group-items group)])
        (let* ([loc (destruction-loc item)]
               [x (loc-x loc)]
               [occ (destruction-occ item)])
          (when (and (fuel? occ)
                     (>= x x-start)
                     (< x x-end))
            (set! retval #t)))))
    retval)
  (define (side-is-clear?)
    (define retval : Boolean #t)
    (for ([x (in-range x-start x-end 1)])
      (for ([y (in-range (state-height state))])
        (when (fuel? (state-get state (make-loc x y)))
          (set! retval #f))))
    retval)
  (define (drop)
    (let* ([height (floor-height state)]
           [drop-amount (min (sub1 height) max-drop-amount)])
      (if (> drop-amount 0)
          (let* ([grid (state-grid state)]
                 [mod! (grid-modifier grid)]
                 [_ (shift grid mod! #:amount drop-amount #:direction 'd #:remove? ground?)]
                 [grid (or (mod! 'build)
                           (fail "couldn't drop floor"))])
            (struct-copy State state
                         [grid grid]))
          state)))
  (and (> max-drop-amount 0)
       ; This left/right split only makes sense for wave layout.
       ; We could update it to work for standard layouts, but not today.
       (equal? 'wave (game-settings-layout:mode settings))
       (destroyed-a-fuel?)
       (side-is-clear?)
       (drop)))


(define normal-ground (make-ground))

(: apply-penalty (-> State (U #f State)))
; If the countdown has reached zero, apply a penalty and reset the countdown.
(define (apply-penalty state)
  (let* ([settings (state-settings state)]
         [ps (state-penalty-state state)]
         [countdown (penalty-state-countdown ps)]
         #:break (when (countdown . > . 0)
                   #f)
         [state : State
                (case (game-settings-penalty:type settings)
                  [(#f) state]
                  [(dump-contaminant)
                   (add-dump state make-contaminant)]
                  [(dump-catalyst)
                   (add-dump state (lambda ([c : Color])
                                     (make-catalyst c #f)))]
                  [(dump-fuel)
                   (add-dump state (lambda ([c : Color])
                                     (make-fuel c #t)))]
                  [(ground) (state-advance state)])]
         [resistance (game-settings-penalty:resistance settings)]
         ; After adding resistance back, countdown might still be negative,
         ; which is OK! We want back-to-back penalties to be possible.
         [countdown (+ countdown resistance)]
         [ps (struct-copy PenaltyState ps
                          [countdown countdown])]
         [state (struct-copy State state
                             [penalty-state ps])])
    state))

(: add-dump (-> State (-> Color Occupant) State))
; Drop a new occupant onto the grid in order to hinder the player.
(define (add-dump state make-occupant)
  (define grid (state-grid state))
  (define max-y (sub1 (grid-height grid)))
  ; make a deterministic yet disposable PRNG so that we don't affect the spawn queue
  (define temp-prng (let* ([ss (state-spawn-state state)]
                           [rand-vec (spawn-state-rand-vec ss)])
                      (vector->pseudo-random-generator rand-vec)))

  (: peaks (-> Grid (Listof Loc)))
  ; Returns the tallest occupied location in each column.
  ; If the column is completely blank, the y-coordinate will be -1.
  (define (peaks grid)
    (let* ([width (grid-width grid)]
           [x-coords : (Sequenceof Integer)
                     (if (= 16 width)
                         ; assuming 16-width wave layout
                         '(3 4 5 6 9 10 11 12)
                         ; assuming 8-width standard layout
                         (in-range width))])
      (for/list ([x : Integer x-coords])
        (for/fold ([top-y : (U #f Integer) #f]
                   #:result (make-loc x (or top-y -1)))
                  ([y : Integer (in-range max-y -1 -1)])
          (or top-y
              (and (grid-get grid (make-loc x y)) y))))))

  ; Group peaks by y-coordinate, then sort from shortest to tallest
  (define groups : (Listof (Listof Loc))
    (sort (group-by loc-y (peaks grid))
          (lambda ([a : (Listof Loc)] [b : (Listof Loc)])
            (< (loc-y (car a)) (loc-y (car b))))))

  (define peak-a : Loc
    ; a peak from the shortest group
    (car (car groups)))
  (define peak-b : Loc
    ; a peak from the second-shortest group (unless there is only one group)
    (if (pair? (cdr groups))
        (car (cadr groups))
        (last (car groups))))

  (: adjacent-colors (-> Loc Direction (Pairof (U #f Color) Integer)))
  ; From the given location (which is assumed to be vacant), returns the count
  ; of "how many consecutive neighbors share the same color?"
  ; Skips over vacant and uncolored locations.
  ; For example, if this returns (cons red 3) then 3 consecutive neighbors in the
  ; given direction are red, and dropping a red into this location is likely to
  ; benefit the player rather than penalize them.
  (define (adjacent-colors given-loc dir)
    (define loc (loc-neighbor given-loc dir))
    (if (not (in-bounds? grid loc))
        (cons #f 0)
        (let* ([occ (grid-get grid loc)]
               [color (and occ (occupant-color occ))]
               [peek (adjacent-colors loc dir)])
          (cond
            [(not color) peek]
            [(car peek)
             (if (equal? (car peek) color)
                 (cons color (add1 (cdr peek)))
                 (cons color 1))]
            [else (cons color 1)]))))

  (: choose-painful-color (-> Loc Color))
  ; Attempts to choose the color that will most hinder the player
  (define (choose-painful-color loc)
    (define down (adjacent-colors loc 'd))
    (define-values (horiz-a horiz-b)
      (let ([left (adjacent-colors loc 'l)]
            [right (adjacent-colors loc 'r)])
        (if (> (cdr right) (cdr left))
            (values right left)
            (values left right))))
    (define colors '(r y b))
    (define (remove! color)
      (when (and color (pair? (cdr colors)))
        (set! colors (remove color colors))))
    (remove! (car down))
    (remove! (car horiz-a))
    (remove! (car horiz-b))
    (list-ref colors
              (random (length colors) temp-prng)))

  (define mod! (grid-modifier grid))
  (for ([peak : Loc (list peak-a peak-b)])
    (let* ([spawn-loc (make-loc (loc-x peak) max-y)]
           [color (choose-painful-color (loc-neighbor peak 'u))]
           [occ (make-occupant color)])
      (mod! 'set spawn-loc occ)))
  (let ([new-grid (mod! 'build)])
    (if new-grid
        (struct-copy State state
                     [grid new-grid])
        (struct-copy State state
                     [game-over? 'died-fuel-generation]))))

(: shift (->* (Grid GridModifier)
              (#:amount Integer #:direction Direction
               #:remove? (U #f (-> Occupant Any)))
              Any))
; Repositions all occupants on the grid.
; If an occupant would be shifted off the grid, we consult the `remove?` predicate
; to know whether it is acceptable to remove that occupant.
; If it is not acceptable, then the modifier will be left in a state such that
; the next call to `(mod! 'build)` will return #f.
(define (shift grid mod! #:amount [amount 1] #:direction [direction 'r] #:remove? [remove? #f])
  (define h (grid-height grid))
  (define w (grid-width grid))
  (define ij->loc : (-> Integer Integer Loc)
    (case direction
      [(u d) (lambda (i j) (make-loc j i))]
      [(l r) (lambda (i j) (make-loc i j))]))
  ; From i-start to i-break are the rows/columns that will be shifted off the screen.
  ; From i-break to i-end are the rest of the rows/columns.
  (define-values (i-start i-break i-end i-step)
    (case direction
      [(d) (values 0 amount h 1)]
      [(l) (values 0 amount w 1)]
      [(u) (values (sub1 h) (- h amount 1) -1 -1)]
      [(r) (values (sub1 w) (- w amount 1) -1 -1)]))
  (define-values (j-start j-end j-step)
    (case direction
      [(u d) (values 0 w 1)]
      [(l r) (values 0 h 1)]))
  (define +- (case direction
               [(d l) -]
               [(u r) +]))
  (when remove?
    (for ([i (in-range i-start i-break i-step)])
      (for ([j (in-range j-start j-end j-step)])
        (let* ([loc (ij->loc i j)]
               [occ (grid-get grid loc)])
          (when occ
            (if (remove? occ)
                (mod! 'remove loc)
                ; this should force a game over:
                (mod! 'set (ij->loc (+- i amount) j) occ)))))))
  (for ([i (in-range i-break i-end i-step)])
    (for ([j (in-range j-start j-end j-step)])
      (let* ([loc (ij->loc i j)]
             [occ (grid-get grid loc)])
        (when occ
          (let ([new-i (+- i amount)])
            (mod! 'remove loc)
            (mod! 'set (ij->loc new-i j) occ)))))))

(: state-advance (-> State State))
(define (state-advance state)
  (let* ([grid (state-grid state)]
         [width (grid-width grid)]
         [height (grid-height grid)]
         [mod! (grid-modifier grid)]
         [_ (shift grid mod! #:direction 'u #:remove? (lambda (o) (not (fuel? o))))]
         [_ (for ([x (in-range width)])
              (mod! 'set (make-loc x 0) normal-ground))]
         [grid (mod! 'build)]
         #:break (when (not grid)
                   (struct-copy State state
                                [game-over? 'died-advance]))
         [state (struct-copy State state
                             [grid grid])]
         [grid (cleanup-top grid fuel?)]
         #:break (when (not grid)
                   (struct-copy State state
                                [game-over? 'died-advance]))
         )
    (struct-copy State state
                 [grid grid])))

(: state-waiting-frame (-> State Integer State))
(define (state-waiting-frame state energy-cost)
  (let* ([stats (state-stats state)]
         [waiting-frames (stats-waiting-frames stats)]
         [waiting-energy (stats-waiting-energy stats)]
         [stats (struct-copy Stats stats
                             [waiting-frames (add1 waiting-frames)]
                             [waiting-energy (+ energy-cost waiting-energy)])]
         [state (struct-copy State state
                             [stats stats])])
    (state-reduce state #:energy energy-cost)))

(: state-reduce (->* (State) (#:energy Integer #:penalty Integer) State))
(define (state-reduce state #:energy [energy-cost 0] #:penalty [penalty-cost 0])
  (let* ([energy (state-energy state)]
         [energy (- energy energy-cost)]
         [game-over? (and (<= energy 0)
                          'lost-energy)]
         [ps (state-penalty-state state)]
         [ps (if (= 0 penalty-cost)
                 ps
                 (struct-copy PenaltyState ps
                              [countdown (- (penalty-state-countdown ps)
                                            penalty-cost)]))]
         [state (struct-copy State state
                             [energy energy]
                             [penalty-state ps]
                             [game-over? game-over?])])
    state))


(: state-apply-action (-> State Action (U #f State)))
(define (state-apply-action state action)
  (match action
    [(move dir)
     (state-move state dir)]
    [(jump dir)
     (state-jump state dir)]
    [(rotate clockwise?)
     (state-rotate state clockwise?)]
    [(plummet)
     (state-plummet state)]
    [(burst)
     (state-burst state)]
    [(tick)
     (state-tick state)]
    [(take-damage amount)
     (state-reduce state #:penalty amount)]
    [else
     (fail "incomplete match" action)]))

; Apply each action to the starting state to produce an end state.
(: state-apply (-> State (Listof Action) (U (Pairof State (Listof Event))
                                            (Pairof #f Any))))
(define (state-apply begin-state actions)
  (: go (-> (U (Pairof State (Listof Event))
               (Pairof #f Any))))
  (define (go)
    (define end-state
      (for/fold ([state : (U #f State) begin-state])
                ([action actions])
        #:break (not state)
        (state-apply-action state action)))
    (cond
      [(not end-state)
       (cons #f "actions did not produce a valid state")]
      [else
       (cons end-state (reverse (or (event-collector)
                                    (fail "event-collector missing"))))]))
  (if (state-game-over? begin-state)
      (cons #f "game is over")
      (parameterize ([event-collector (list)])
        (go))))

(module+ test
  (void (state-apply (make-empty-state 4 4) (list))))

(: make-initial-state (-> GameSettings State))
(define (make-initial-state settings)
  (let* ([wave-mode? (equal? 'wave (game-settings-layout:mode settings))]
         [width (if wave-mode? 16 8)]
         [height (if wave-mode? 16 22)]
         [state (make-empty-state width height settings)]
         [state (add-ground state)]
         [state (if wave-mode?
                    state
                    (or (add-fuel/standard-layout state)
                        (fail "failed to place fuels (were zero waves requested?)")))])
    state))

(: add-ground (-> State State))
(define (add-ground state)
  (let* ([grid (state-grid state)]
         [width (grid-width grid)]
         [mod! (grid-modifier grid)])
    (for ([x (in-range width)])
      (mod! 'set (make-loc x 0) normal-ground))
    (struct-copy State state
                 [grid (or (mod! 'build)
                           (fail "add-ground failed"))])))

(: add-fuel/standard-layout (-> State (U #f State)))
; Checks that there are no fuels left and remaining-wave-count is > 0.
; If so, clears the grid and generates a new standard layout.
(define (add-fuel/standard-layout state)
  (define y-offset (floor-height state))
  (define fuel-count (game-settings-layout:fuel-count (state-settings state)))
  (define fuel-height (game-settings-layout:fuel-height (state-settings state)))

  ; The general strategy here is:
  ; 1) Create a "pattern" that specifies the locations that should have a fuel.
  ; 2) Now assign the colors for those locations.
  ; Colors are assigned in batches of peaks, where a "peak" is the highest loc in a column
  ; that should have a fuel according to the pattern, but does not have a fuel yet.
  ; Each batch of peaks must have at least one of each color (if possible).
  ; The goal is to avoid having large areas with no red fuel, for example.
  ; This is especially important at the top of a new game.
  ;
  ; This strategy means that we are not promising an even distribution of colors,
  ; but it plays really well anyway (IMO).

  (: create-pattern (All (a) (-> Pseudo-Random-Generator (Vectorof Boolean) Integer (Vectorof Boolean))))
  ; Sets `count` random indexes of the given vector to #t.
  ; Does not validate that count is <= the size of the vector.
  (define (create-pattern prng vec count)
    (if (<= count 0)
        vec
        (let* ([index (random (vector-length vec) prng)])
          (if (vector-ref vec index)
              (create-pattern prng vec count)
              (begin
                (vector-set! vec index #t)
                (create-pattern prng vec (sub1 count)))))))

  (: find-peaks (-> Grid (Vectorof Boolean) (Listof Loc)))
  ; For each column in the grid, find the tallest loc that should be populated
  ; (according to `pattern`) but is not populated yet.
  (define (find-peaks grid pattern)
    (let ([width (grid-width grid)]
          [peaks : (Listof Loc) (list)])
      (for ([x (in-range width)])
        (let ([peak : (U #f Loc) #f])
          (for ([y (in-range fuel-height)])
            (let ([loc (make-loc x (+ y y-offset))]
                  [index (+ x (* width y))])
              (when (and (vector-ref pattern index)
                         (not (grid-get grid loc)))
                (set! peak loc))))
          (let ([peak peak])
            (when peak
              (set! peaks (cons peak peaks))))))
      peaks))

  (: choose-colors (-> Pseudo-Random-Generator Integer (Listof Color)))
  ; Creates and returns a list of `count` random colors.
  ; Ensures that each color occurs at least once (if possible).
  (define (choose-colors prng count)
    (let* ([part-a (shuffle '(r y b) prng)]
           [part-b (shuffle '(r r y y b b) prng)])
      (take (append part-a part-b) count)))

  (: add-fuels (-> Pseudo-Random-Generator Grid (Vectorof Boolean) Grid))
  ; Add fuels to the given grid according to the given pattern.
  ; Do this in batches of peaks.
  (define (add-fuels prng grid pattern)
    (let* ([peaks (find-peaks grid pattern)]
           #:break (when (empty? peaks)
                     grid)
           [colors (choose-colors prng (length peaks))]
           [mod! (grid-modifier grid)]
           [_ (for ([loc peaks]
                    [color colors])
                (mod! 'set loc (make-fuel color #f)))]
           [new-grid (or (mod! 'build)
                         (fail "add-fuels failed"))]
           [(_ groups)
            ; Destroy groups of 3
            (grid-destroy new-grid 3)]
           #:break (when (not (empty? groups))
                     ; Something was destroyed, discard result and try again
                     (add-fuels prng grid pattern)))
      (add-fuels prng new-grid pattern)))

  ; main body
  (let* ([ls (state-layout-state state)]
         [wave-count (layout-state-remaining-wave-count ls)]
         #:break (when (= 0 (or wave-count 1))
                   #f)
         [grid (state-grid state)]
         [fuel-exists? (> (grid-count grid fuel?) 0)]
         #:break (when fuel-exists?
                   #f)
         [grid (grid-clear grid (lambda (occ) (not (ground? occ))))]
         [width (grid-width grid)]
         ; Build pattern
         [pattern-size (* width fuel-height)]
         #:break (when (> fuel-count pattern-size)
                   (fail "cannot fit fuels" width fuel-height fuel-count))
         [rand-vec (layout-state-rand-vec ls)]
         [prng (vector->prng rand-vec)]
         [pattern : (Vectorof Boolean)
                  (make-vector pattern-size #f)]
         [pattern (create-pattern prng pattern fuel-count)]
         ; Apply pattern
         [grid (add-fuels prng grid pattern)]
         [rand-vec (prng->vector prng)]
         [ls (struct-copy LayoutState ls
                          [remaining-wave-count (and wave-count
                                                     (sub1 wave-count))]
                          [rand-vec rand-vec])])
    (struct-copy State state
                 [layout-state ls]
                 [grid grid])))

; Transposes rows to columns and columns to rows.
(: transpose (All (A) (-> (Listof (Listof A)) (Listof (Listof A)))))
(define (transpose lists)
  (define :car (ann car (-> (Listof A) A)))
  (define :cdr (ann cdr (-> (Listof A) (Listof A))))
  (cond
    [(empty? lists) (list)]
    [(empty? (first lists)) (list)]
    [else
     (let ([a : (Listof A) (map :car lists)]
           [b : (Listof (Listof A)) (map :cdr lists)])
       (cons a (transpose b)))]))

(: state-width (-> State Dimension))
(define (state-width state)
  (grid-width (state-grid state)))

(: state-height (-> State Dimension))
(define (state-height state)
  (grid-height (state-grid state)))
