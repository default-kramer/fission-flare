#lang typed/racket/base

(provide (all-defined-out)
         write-dto)

(require (for-syntax typed/racket/base)
         "../typed-utils.rkt"
         "data-helpers.rkt"
         "../util/splice.rkt"
         )

(module+ test
  (require typed/rackunit))

; A Loc ("Location") is a pair of (x, y) coordinates
(define-type Loc (Pairof Integer Integer))

(: make-loc (-> Integer Integer Loc))
(define make-loc cons)

(: loc-x (-> Loc Integer))
(define loc-x car)

(: loc-y (-> Loc Integer))
(define loc-y cdr)

; red, yellow, blue, #f = blank
(define-type Color (U 'r 'y 'b #f))

; up, right, down, left
(define-type Direction (U 'u 'r 'd 'l))

(: loc-neighbor (-> Loc Direction Loc))
(define (loc-neighbor loc direction)
  (let ([x (loc-x loc)]
        [y (loc-y loc)])
    (case direction
      [(u) (make-loc x (add1 y))]
      [(r) (make-loc (add1 x) y)]
      [(d) (make-loc x (sub1 y))]
      [(l) (make-loc (sub1 x) y)])))

; An occupant is anything that can occupy a 1x1 cell of the grid.
(define-type Occupant (U Catalyst Fuel Ground Contaminant))

(define occupant? (make-predicate Occupant))

; Catalysts are the occupants that the player can control.
; They have a partner when spawned, but the partner can be destroyed.
(ss Catalyst catalyst ([color : Color]
                       ; direction is the location of my partner relative to my location
                       [direction : (U #f Direction)]))

; Destroying fuel is usually the objective of the game.
(ss Fuel fuel ([color : Color]
               [falling? : Boolean]))

; A contaminant behaves like a catatlyst without a partner, except that
; it is harder to destroy vertically (requires 5 instead of 4) and easier
; to destroy horizontally (requires 3 instead of 4).
; Also, when a contaminant is destroyed it usually prevents the reward for the
; destruction group(s) that contain it.
(ss Contaminant contaminant ([color : Color]))

; This project uses the terms "ground" and "floor" somewhat interchangeably.
; Ground is used to block other occupants from falling off the grid.
; Certain game settings can raise the floor as a penalty.
(ss Ground ground ())

(: occupant-color (-> Occupant Color))
(define (occupant-color occ)
  (cond
    [(catalyst? occ) (catalyst-color occ)]
    [(fuel? occ) (fuel-color occ)]
    [(contaminant? occ) (contaminant-color occ)]
    [(ground? occ) #f]
    ))

(: can-burst? (-> Any Boolean))
(define (can-burst? occ)
  (and (catalyst? occ)
       (not (occupant-color occ))))

(define-type GridHT (Immutable-HashTable Loc Occupant))

(define-type Dimension Positive-Byte)

(ss Grid grid ([ht : GridHT]
               [width : Dimension]
               [height : Dimension]))

(: grid-get (-> Grid Loc (U #f Occupant)))
(define (grid-get grid loc)
  (hash-ref (grid-ht grid) loc #f))

(: make-empty-grid (-> Dimension Dimension Grid))
(define (make-empty-grid width height)
  (make-grid (ann (hash) GridHT) width height))

; This struct captures the destruction of a single occupant:
(ss Destruction destruction ([loc : Loc]
                             [occ : Occupant]
                             ; Given (for example) a horizontal group of 4 items, the delay of
                             ; the leftmost item will be 0 and the rightmost item will be 3.
                             ; This is used to animate the ripple effect.
                             [animation-delay : Fixnum]))

; This struct captures a group of adjacent color-matched occupants.
(ss DestructionGroup destruction-group ([key : Color]
                                        [items : (Listof Destruction)]))

(: destruction-group-locs (-> DestructionGroup (Listof Loc)))
(define (destruction-group-locs dg)
  (map destruction-loc (destruction-group-items dg)))

; A "mover" contains the two occupants that are able to moved by the player.
; Note that the occupants are not placed onto the grid until they land.
; The grid is totally unaware of the mover.
(ss Mover mover ([occ-a : Catalyst]
                 [loc-a : Loc]
                 [occ-b : Catalyst]
                 [loc-b : Loc]))

; I thought the penalty-state would need its own PRNG, but apparently not yet...
(ss PenaltyState penalty-state
    (; When `countdown` reaches zero, a penalty is activated.
     ; The penalty type is in the game settings.
     [countdown : Integer]))

; used by vector->pseudo-random-generator and pseudo-random-generator->vector
(define-type PrngState (Immutable-Vector
                        Positive-Integer
                        Positive-Integer
                        Positive-Integer
                        Positive-Integer
                        Positive-Integer
                        Positive-Integer))

(define-type LaxPrngState (U PrngState (Mutable-Vector
                                        Positive-Integer
                                        Positive-Integer
                                        Positive-Integer
                                        Positive-Integer
                                        Positive-Integer
                                        Positive-Integer)))

; Rebuild the vector to work around https://github.com/racket/typed-racket/issues/1063
(: vector->prng (-> LaxPrngState Pseudo-Random-Generator))
(define (vector->prng vec)
  (: de-chaperone (-> LaxPrngState LaxPrngState))
  (define (de-chaperone v)
    (if (chaperone? v)
        (vector (vector-ref vec 0)
                (vector-ref vec 1)
                (vector-ref vec 2)
                (vector-ref vec 3)
                (vector-ref vec 4)
                (vector-ref vec 5))
        v))
  (vector->pseudo-random-generator (de-chaperone vec)))

(: prng->vector (-> Pseudo-Random-Generator PrngState))
(define (prng->vector prng)
  (define vec (pseudo-random-generator->vector prng))
  (if (immutable? vec)
      vec
      (vector-immutable (vector-ref vec 0)
                        (vector-ref vec 1)
                        (vector-ref vec 2)
                        (vector-ref vec 3)
                        (vector-ref vec 4)
                        (vector-ref vec 5))))

; For example, the vector '#(0 100 300 600) means that a single is worth 0,
; a double is worth 100, a triple is worth 300, and a quad is worth 600.
; A quint is worth 900 by extrapolation from the last two entries.
(define-type PayoutTable (Immutable-Vectorof Integer))

(: payout-ref (-> PayoutTable Integer Integer))
(define (payout-ref table i)
  ; translate one-based index into zero-based
  (let ([i (sub1 i)]
        [len (vector-length table)])
    (cond
      [(i . < . 0)
       0]
      [(i . < . len)
       (vector-ref table i)]
      [else ; we need to extrapolate
       (let* ([a (sub1 len)]
              [b (sub1 a)]
              [_ (when (b . < . 0)
                   (fail "payout table isn't big enough to extrapolate" table i))]
              [a (vector-ref table a)]
              [b (vector-ref table b)]
              [increment (a . - . b)]
              [_ (when (increment . < . 0)
                   (fail "payout table has negative increment" table))])
         (+ a (* increment (add1 (i . - . len)))))])))

(module+ test
  (let ([table '#(42 200 300)])
    (check-equal? (payout-ref table 0) 0)
    (check-equal? (payout-ref table 1) 42)
    (check-equal? (payout-ref table 2) 200)
    (check-equal? (payout-ref table 3) 300)
    (check-equal? (payout-ref table 4) 400)
    (check-equal? (payout-ref table 5) 500)))

(begin-for-syntax
  ; gs# means "game settings syntax", format is
  #;([id : Type default-value] ...)
  (define gs#
    #'(; === Layout Settings ===
       [layout:mode : (U 'standard 'wave) 'standard]
       ; standard layout only: How high should fuel be generated?
       [layout:fuel-height : Integer 10]
       ; standard layout only: How many fuels should be generated?
       [layout:fuel-count : Integer 30]
       ; `num-waves` actually applies to both standard and wave layouts.
       ; It's just that for standard layouts we will usually set this value to 1
       ; (meaning "just beat the level") or #f (meaning "time attack, go nuts")
       [layout:num-waves : (U #f Integer) 1]

       ; === Time Attack Settings ===
       ; How should the time be measured?
       ;   'waiting-clock only consumes time while the player is able to make a move.
       ;   'wall-clock consumes time regardless of what is happening.
       ;   #f indicates "not time attack"
       [time-attack:type : (U #f 'waiting-clock 'wall-clock) #f]
       ; How long in seconds should the time attack game last?
       [time-attack:seconds : Integer 600]

       ; === Energy Settings ===
       [energy:initial : Integer 10000]
       ; Energy cannot be restored beyond this limit. (Or can it?)
       ; False indicates that we are (probably) in time attack mode.
       [energy:max : (U #f Integer) 10000]
       ; energy:catalyst-cost reduces energy on each spawn
       [energy:catalyst-cost : Integer 100]
       ; energy:drain-rate reduces energy during each waiting frame
       [energy:drain-rate : Integer 1]
       ; how much energy do combos restore?
       [energy:combo-payout : PayoutTable '#(0 200 500 1000)]
       ; how much energy does a single fuel restore?
       [energy:fuel-value : Integer 100]
       ; how much energy should the horizontal bonus restore?
       [energy:horizontal-bonus-factor : Integer 20]
       ; how much energy should be restored when a wave is completed?
       [energy:wave-completion-bonus : Integer 5000]

       ; === Penalty Settings ===
       ; Which kind of penalty should be generated?
       [penalty:type : (U #f 'ground 'dump-contaminant 'dump-catalyst 'dump-fuel) #f]
       ; `penalty:resistance` is the value at which the penalty countdown starts.
       ; A higher value means that penalties will occur less frequently.
       ; In multiplayer, before the game starts this value will be multiplied by the
       ; number of opponents you are facing.
       [penalty:resistance : Integer 1000]
       ; Each catalyst spawned will subtract this cost from the penalty countdown.
       ; Probably just for single-player games when you want to simulate an opponent attacking you.
       [penalty:catalyst-cost : Integer 0]
       ; wave layout only: How much should the floor be dropped when a wave is cleared?
       [penalty:floor-drop-amount : Integer 4]

       ; === Attack Settings ===
       [attack:combo-payout : PayoutTable #(0 50 100 200 500 1000)]
       [attack:horizontal-bonus-factor : Integer 50]

       ; === Misc Settings ===
       ; The catalyst deck controls the distribution of spawned catalysts.
       ; Implemented as a deck of cards that re-shuffled after it is consumed.
       [misc:catalyst-deck : (U 'blank-12 'blank-24 'noblank-9 'noblank-18) 'blank-12]
       ; If the following setting is true, only groups with at least one fuel
       ; count towards combo payout table lookups.
       ; This is a surefire way to make a "spam empty combos" strategy inviable,
       ; but empty combos are nice on the final seconds of a challenging wave mode game.
       [misc:combos-require-fuel? : Boolean #f]
       ; This random seed should produce determinism for the entire game.
       ; For example, if you start a game with the same settings and same seed and make
       ; the same moves you should always arrive at the same outcome.
       [misc:random-seed : (U #f PrngState) #f]
       ))

  (define-values (gs#field-ids gs#types gs#default-vals)
    (syntax-case gs# (:)
      [([field : Type default-val] ...)
       (values #'(field ...)
               #'(Type ...)
               #'(default-val ...))])))

(with-syntax/splice ([(field-id ...) gs#field-ids]
                     [(Type ...) gs#types]
                     [(pred-id ...) (generate-temporaries gs#types)]
                     [(default-val ...) gs#default-vals]
                     [ooo (quote-syntax ...)])
  (begin
    (ss GameSettings game-settings ([field-id : Type] ...))
    (define default-game-settings (game-settings default-val ...))
    (define pred-id (make-predicate Type))
    ...
    (define-assoc-converters ooo GameSettings game-settings
      settings->assoc assoc->settings ([field-id pred-id] ...))))

(module+ test
  (check-true (equal? (assoc->settings (settings->assoc default-game-settings))
                      default-game-settings)))

; The spawn state will be implemented as a shuffled list of all posibilities
; that respects the desired color distribution.
; It gets its own PRNG to ensure that (in multiplayer) all players will get the
; same sequence of catalysts.
(ss SpawnState spawn-state ([card-stack : (Listof (Pairof Color Color))]
                            [rand-vec : PrngState]))

; The layout state is used when we need to generate the random fuel layouts
; of either standard or wave mode.
; It gets its own PRNG to ensure that (in multiplayer) all players get the same fuel layouts.
(ss LayoutState layout-state
    (; #f indicates "infinite", used for time attack
     [remaining-wave-count : (U #f Integer)]
     [rand-vec : PrngState]))

(ss Stats stats ([spawn-count : Integer] ; how many spawn events
                 [spawn-energy : Integer] ; total energy consumed by spawn events
                 [waiting-frames : Integer] ; how many waiting frames
                 [waiting-energy : Integer] ; total energy consumed by waiting frames
                 [energy-earned : Integer]))

(define initial-stats (stats 0 0 0 0 0))

(ss Combo combo ([groups : (Listof DestructionGroup)]
                 [total-payout : Integer]
                 [explanations : (Listof (List String Integer String))]))

; Represents the game state of one player.
; Multiplayer games will have one state per player.
(ss State state ([grid : Grid]
                 [mover : (U #f Mover)]
                 [spawn-state : SpawnState]
                 [penalty-state : PenaltyState]
                 [energy : Integer]
                 [layout-state : LayoutState]
                 [current-combo : (U #f Combo)]
                 [previous-combo : (U #f Combo)]
                 [settings : GameSettings]
                 [stats : Stats]
                 [game-over? : (U #f 'win 'died-spawn 'died-advance 'died-fuel-generation 'lost-energy 'time-expired)]))

(define (state-remaining-wave-count [state : State])
  (layout-state-remaining-wave-count (state-layout-state state)))

(define (state-max-energy [state : State])
  (game-settings-energy:max (state-settings state)))


; A Frame layers timing-based logic on top of the State.
(define-type FrameCount Integer)

(define-type FrameInfo (U (List 'waiting) ; for user input
                          (List 'bursting) ; drop button being held down for a possible burst
                          (List 'falling)
                          (Pairof 'destroying (Listof DestructionGroup))
                          (Pairof 'spawning (Listof CatalystSpawned))
                          (List 'game-over)))

; How many frames should each of these phases consume?
(ss Timing timing ([fps : FrameCount]
                   [falling : FrameCount]
                   [destroying : FrameCount] ; frame count between destroying the Nth and (N+1)th occupant
                   [bursting : FrameCount] ; for how many frames must the user hold the button down?
                   [spawning : FrameCount]))

; If we run at 30 FPS, then 30 times per second we will call a function (-> Frame Frame)
(ss Frame frame ([state : State]
                 [counter : FrameCount]
                 ; `(car info)` is the frame counter from when this FrameInfo began
                 [info : (Pairof FrameCount FrameInfo)]
                 [timing : Timing]))

(define-syntax-rule (define/union UnionId (TypeId struct-id more ...) ...)
  (begin
    (ss TypeId struct-id more ...)
    ...
    (define-type UnionId (U TypeId ...))))

; Actions
; The initial state plus the sequence of Stamped Actions is enough to replay
; an entire game.
(define/union Action
  (Move move ([dir : Direction]))
  (Jump jump ([dir : Direction]))
  (Rotate rotate ([clockwise? : Boolean]))
  (Plummet plummet ())
  (Burst burst ())
  (Tick tick ())
  (TakeDamage take-damage ([amount : Integer]))
  ; These actions are handled by Frame, not State... Is there a cleaner way?
  (DropKeydown drop-keydown ())
  (DropKeyup drop-keyup ()))

; Events
; We do not transmit Events across the network. We will get the same
; sequence of Events as long as we replay the Stamped Actions correctly.
(define/union Event
  ; The (Stamped ActionPerformed) is used for animating multiplayer
  (ActionPerformed action-performed ([action : Action]))
  (AttackGenerated attack-generated ([amount : Integer]))
  (OccupantsDestroyed occupants-destroyed ([groups : (Listof DestructionGroup)]))
  ; Two events will be generated for a normal spawn (left and right half)
  (CatalystSpawned catalyst-spawned ([catalyst : Catalyst]
                                     [loc : Loc])))

(define event? (make-predicate Event))


; == begin Multiplayer Stuff ==

(define-type Pid Integer) ; player ID
(define-type SnapId Integer) ; snapshot ID

; A stamp can accompany an Action or Event (or anything)
(ss Stamp stamp ([pid : (U #f Pid)]
                 [client-timestamp : (U #f Flonum)]
                 [framestamp : (U #f FrameCount)]))

(define-type (Stamped A) (Pairof Stamp A))

(: stamped-value (All (A) (-> (Stamped A) A)))
(define (stamped-value x)
  (cdr x))

(: stamped-pid (-> (Stamped Any) (U #f Pid)))
(define (stamped-pid x)
  (stamp-pid (car x)))

(: stamped-client-timestamp (-> (Stamped Any) (U #f Flonum)))
(define (stamped-client-timestamp x)
  (stamp-client-timestamp (car x)))

(: stamped-framestamp (-> (Stamped Any) (U #f FrameCount)))
(define (stamped-framestamp x)
  (stamp-framestamp (car x)))

(define stamped? (make-predicate (Stamped Any)))
(define stamped-action? (make-predicate (Stamped Action)))
(define stamped-event? (make-predicate (Stamped Event)))

; The competition type determines how much an individual player is allowed to
; deviate from the server's settings
;   'symmetric - no deviation permitted
;   'handicap - players may adjust payouts only
;   'custom - players may adjust anything they want
(define-type CompetitionType (U 'symmetric 'handicap 'custom))

(ss MultiplayerSettings multiplayer-settings
    ; A multiplayer lobby always has server-side GameSettings, but individual
    ; players may be allowed to deviate from those settings.
    ([game-settings : GameSettings]
     ; win-condition defines how the winner is chosen:
     ;   'speed - The player who finishes first wins (lowest frame count)
     ;   'energy - The player who finishes with the most energy wins
     [win-condition : (U 'speed 'energy)]
     [competition-type : CompetitionType]
     ; The viewmodel is an assoc to help us show the same UI on the client as
     ; on the host. (Because `UI -> GameSettings` is not reversible.)
     [viewmodel : Any]))

(define default-multiplayer-settings
  (make-multiplayer-settings default-game-settings 'speed 'symmetric '()))

; A token is like a userid+password for a single game
(ss Token token ([pid : Pid] [secret : String]))

; should probably add PID to this struct:
(ss PlayerSettings player-settings ([name : String]
                                    [ready? : Boolean]
                                    [game-settings : GameSettings]))

(ss PlayerInfo player-info ([pid : Pid]
                            [settings : PlayerSettings]
                            ; The following field remains #f until the game starts.
                            ; Once non-false, it will never change again.
                            [start-game-payload : (U #f (List GameSettings MultiplayerSettings))]))

(define (player-info-name [pi : PlayerInfo])
  (player-settings-name (player-info-settings pi)))

(define (player-info-ready? [pi : PlayerInfo])
  (player-settings-ready? (player-info-settings pi)))

(define/union Request
  (rq:GetPid ; asks the server to assign me a new player id
   rq:get-pid ())
  (rq:BecomePid ; used to resume a previously-assigned player id
   rq:become-pid ([token : Token]))
  (rq:PollLobby ; pushes my lobby settings to the server
   rq:poll-lobby ([settings : PlayerSettings]))
  (rq:Sync ; pushes my local game state updates to the server
   rq:sync ([from-snapshot-id : SnapId]
            [actions : (Listof (Stamped Action))]
            ; the client's current frame
            [frame-count : FrameCount]
            ; the `equal-hash-code` of the client's current frame
            [frame-hash-code : Fixnum])))

(define/union Response
  (rs:GotPid ; response to `get-pid` or `become-pid`
   rs:got-pid ([token : Token]))
  (rs:PollLobby
   rs:poll-lobby ([players : (Listof PlayerInfo)]
                  [mp-settings : MultiplayerSettings]))
  (rs:Sync rs:sync ([new-snapshot-id : SnapId]
                    [homework : (Listof Action)]
                    [other-player-actions : (Listof (Stamped Action))])))

(define rq? (make-predicate Request))
(define rs? (make-predicate Response))


; Now we can define `datum->dto` which "deserializes" a datum into one of the
; following objects (or a primitive like list, hash, pair, vector).
(with-syntax/splice ([(game-setting-val ...) gs#default-vals])
  (define-deserializer datum->dto the
    (burst)
    (drop-keydown)
    (drop-keyup)
    (game-settings game-setting-val ...)
    (jump 'l)
    (move 'd)
    (multiplayer-settings (the game-settings) 'speed 'symmetric '())
    (player-info 1 (the player-settings)
                 (list (the game-settings) (the multiplayer-settings)))
    (player-settings "name" #t (the game-settings))
    (plummet)
    (rotate #t)
    (stamp 1 2.0 3)
    (take-damage 1)
    (token 0 "secret")
    ; requests
    (rq:become-pid (the token))
    (rq:get-pid)
    (rq:poll-lobby (the player-settings))
    (rq:sync 1 (list (cons (the stamp) (the rotate))
                     (cons (the stamp) (the move)))
             2 3)
    ; responses
    (rs:got-pid (the token))
    (rs:poll-lobby (list (the player-info))
                   (the multiplayer-settings))
    (rs:sync 1 (list (the rotate) (the move))
             (list (cons (the stamp) (the rotate))
                   (cons (the stamp) (the plummet))))
    ))

(: read-dto (->* () (Input-Port) Any))
(define (read-dto [in (current-input-port)])
  (define datum (read-datum in))
  (if (eof-object? datum)
      datum
      (datum->dto datum)))
