#lang racket/gui

(provide build-settings setup-panel%)

#;("Methods of setup-panel%"
   (make-game-settings)  ; (-> GameSettings)
   (make-mp-settings)    ; (-> MultiplayerSettings)
   ; set-constraints returns #t if something changed and #f otherwise
   (set-constraints mps) ; (-> MultiplayerSettings Boolean)
   )

(require "../util/assert.rkt"
         "../core.rkt"
         )

; A handicap is a percentage that boosts your attack power.
; It is an integer; the lowest value is 100 (not 1.0) which gives you no boost.
(define no-handicap 100)
(define handicap-choices
  '#(100 110 120 135 150 175 200 250 300 400 500))

(define (hc-scale handicap x)
  (define factor (/ handicap 100))
  (define (mult val)
    (inexact->exact (ceiling (* factor val))))
  (cond
    [(vector? x)
     (vector->immutable-vector (vector-map mult x))]
    [(number? x)
     (mult x)]
    [else
     (fail "incomplete cond" x)]))

; Builds game settings assuming not time-attack mode
(define (build-settings-2 mode level
                          #:blanks? [blanks? #t]
                          #:penalty-type [penalty-type #f]
                          #:handicap [handicap no-handicap])
  (let* ([energy:combo-payout (hc-scale handicap '#(0 200 500 1000 2000))]
         [energy:horizontal-bonus-factor (hc-scale handicap 20)]
         [energy:drain-rate (+ 1 (quotient (+ level 9) 10))]
         [deck (if blanks? 'blank-12 'noblank-9)]
         [catalyst-cost (+ 50 (* level 5))]
         [standard-fuel-count (* 3 (add1 (min 27 level)))]
         [standard-energy (+ 4000 (* 400 (+ standard-fuel-count 6)))]
         [attack:combo-payout (hc-scale handicap '#(0 50 100 200 500 1000))]
         [attack:horizontal-bonus-factor (hc-scale handicap 50)]
         [penalty:type (case penalty-type
                         [(0) #f]
                         [(1) 'dump-contaminant]
                         [(2) 'dump-catalyst]
                         [(3) 'dump-fuel]
                         [else penalty-type])]
         [default-game-settings (struct-copy GameSettings default-game-settings
                                             [misc:catalyst-deck deck]
                                             [energy:combo-payout energy:combo-payout]
                                             [energy:horizontal-bonus-factor energy:horizontal-bonus-factor]
                                             [energy:drain-rate energy:drain-rate]
                                             [energy:catalyst-cost catalyst-cost]
                                             [attack:combo-payout attack:combo-payout]
                                             [attack:horizontal-bonus-factor attack:horizontal-bonus-factor]
                                             [penalty:type penalty:type])])
    (case mode
      [(0 standard)
       (struct-copy GameSettings default-game-settings
                    [layout:mode 'standard]
                    [layout:num-waves 1]
                    [layout:fuel-count (* 3 (add1 (min 27 level)))]
                    [layout:fuel-height
                     (case level
                       [(0 1 2 3 4 5 6 7 8 9 10 11 12 13)
                        10]
                       [(14 15)
                        11]
                       [(16 17)
                        12]
                       [else 13])]
                    [energy:initial standard-energy]
                    [energy:max standard-energy])]
      [(1 wave)
       (struct-copy GameSettings default-game-settings
                    [layout:mode 'wave]
                    [layout:num-waves 5]
                    [energy:fuel-value 50]
                    [energy:wave-completion-bonus 3000])]
      [else
       (fail "unexpected mode:" mode)])))

(define (build-settings mode level time-attack-type
                        #:constraints [constraints #f]
                        #:blanks? [blanks? #t]
                        #:penalty-type [penalty-type #f]
                        #:handicap [handicap no-handicap])
  (let* ([settings (build-settings-2 mode level
                                     #:blanks? blanks?
                                     #:penalty-type penalty-type
                                     #:handicap handicap)]
         [type (case time-attack-type
                 [(0 #f) #f]
                 [(1 wall-clock) 'wall-clock]
                 [(2 waiting-clock) 'waiting-clock])]
         [settings (if type
                       (struct-copy GameSettings settings
                                    [energy:catalyst-cost 0]
                                    [energy:drain-rate 0]
                                    [time-attack:type type]
                                    [energy:initial 1]
                                    [layout:num-waves #f])
                       settings)]
         [settings (if constraints
                       (merge-settings constraints settings)
                       settings)])
    settings))

(define (build-server-settings win-condition competition-type game-settings viewmodel)
  (let ([win-condition (case win-condition
                         [(0 speed) 'speed]
                         [(1 energy) 'energy]
                         [else (fail "bad win-condition" win-condition)])]
        [competition-type (case competition-type
                            [(0 symmetric) 'symmetric]
                            [(1 handicap) 'handicap]
                            [(2 custom) 'custom]
                            [else (fail "bad competition-type" competition-type)])])
    (make-multiplayer-settings
     game-settings win-condition competition-type viewmodel)))

(define (settings->string settings)
  (let ([port (open-output-string)])
    (parameterize ([pretty-print-columns 100])
      (pretty-print (settings->assoc settings) port 1))
    (get-output-string port)))

(define choice? (is-a?/c choice%))

(define (get-value control)
  (cond
    [(not control)
     #f]
    [(choice? control)
     (send control get-selection)]
    [else
     (send control get-value)]))

(define (set-value control value)
  (cond
    [(not control) (void)]
    [(choice? control)
     (send control set-selection value)]
    [else
     (send control set-value value)]))

; IMO it is not obvious when a slider% is disabled.
; So instead, we will wrap every control in its own panel% and disable that.
(define (set-enabled control enabled?)
  (let* ([parent (send control get-parent)]
         [kids (send parent get-children)])
    (when (not (equal? kids (list control)))
      (fail "unexpected siblings, cannot disable" control))
    (send parent enable enabled?)))

(define (enabled? control)
  (and control
       (send control is-enabled?)
       (send (send control get-parent) is-enabled?)))

(define larger-control-font
  (let ([f normal-control-font])
    (make-font #:size (* 1.2 (send f get-size))
               #:face (send f get-face)
               #:family (send f get-family)
               #:style (send f get-style)
               #:weight (send f get-weight)
               #:smoothing (send f get-smoothing)
               #:hinting (send f get-hinting))))

(define setup-panel%
  (class vertical-panel%
    ; constraints : (or/c #f multiplayer-settings?)
    ; A constraining template for clients of a multiplayer game.
    (init-field [constraints #f])
    ; host-mode? : enables settings that are available only to the host
    (init-field [host-mode? #f])
    ; The following callbacks are (or/c #f (-> setup-panel% any)).
    (init-field [start-game-callback #f]
                [setting-changed-callback #f])
    (super-new [stretchable-width #f]
               [stretchable-height #f]
               [spacing 15])

    ; Host mode should be totally unconstrained
    (assert (not (and constraints host-mode?)))
    (define is-multiplayer? (or host-mode? constraints))

    (define (update-custom-spec settings)
      (let ([str (settings->string settings)])
        ;(println str)
        (send @custom-spec set-value str)
        ; This `refresh` is necessary for some reason. Maybe a Racket bug?
        (send @custom-spec refresh)))

    (define (onchange)
      (when setting-changed-callback
        (setting-changed-callback this)))

    (define (respec . args)
      (let* ([mode (send @mode get-selection)]
             [level (send @level get-value)]
             [blanks? (send @blanks-enabled get-value)]
             [handicap (and (enabled? @handicap)
                            (send @handicap get-value))]
             [handicap (if handicap
                           (vector-ref handicap-choices handicap)
                           no-handicap)]
             [penalty-type (send @penalty-type get-selection)]
             [time-attack-type (send @time-attack-type get-selection)]
             [settings (build-settings mode level time-attack-type
                                       #:constraints constraints
                                       #:handicap handicap
                                       #:penalty-type penalty-type
                                       #:blanks? blanks?)])
        (update-custom-spec settings)
        (onchange)))

    (define (custom-spec-changed . args)
      (define game-settings
        (with-handlers ([any/c (lambda args #f)])
          (make-game-settings)))
      (when game-settings
        (onchange)))

    {begin
      (define *parent this)
      (define-syntax-rule (add class% opts ...)
        (let ([wrapper (new panel% [parent *parent])])
          (new class%
               [parent wrapper]
               opts ...)))
      (define-syntax-rule (def id class% opts ...)
        (define id (add class% opts ...)))
      (define @win-condition
        (and is-multiplayer?
             (add choice%
                  ; All choices will pad the label with some spaces
                  [label "Win Condition  "]
                  [choices '("Speed" "Energy")]
                  [callback respec])))
      (define @competition-type
        (and is-multiplayer?
             (add choice%
                  [label "Competition Type  "]
                  [choices '("Symmetric" "Handicap" "Custom")]
                  [callback respec])))
      (def @mode choice%
        [label "Layout  "]
        [choices '("Standard" "Wave")]
        [callback respec])
      (def @level slider%
        [label "Level"]
        [min-value 1]
        [max-value 40]
        [callback respec])
      (def @time-attack-type choice%
        [label "Time Attack  "]
        [choices '("Disabled" "Wall Clock" "Waiting Clock")]
        [callback respec])
      (def @blanks-enabled check-box%
        [label "Blank Catalysts?"]
        [value #t]
        [callback respec])
      (def @penalty-type choice%
        [label "Penalty Type  "]
        [choices '("None" "Contaminant" "Catalyst" "Fuel")]
        [callback respec])
      (define @handicap
        (and constraints
             (not host-mode?)
             (add slider%
                  [label "Handicap"]
                  [min-value 0]
                  [max-value (sub1 (vector-length handicap-choices))]
                  [callback respec])))
      (define @start-game
        (and start-game-callback
             (add button%
                  [label "Start Game"]
                  [vert-margin 10]
                  [font larger-control-font]
                  [callback (lambda args (start-game-callback this))])))
      (add message%
           [label "Expert settings. Modifying might crash the game:"])
      (def @custom-spec text-field%
        [label ""]
        [min-height 350]
        [min-width 280]
        [callback custom-spec-changed]
        [stretchable-width #f])
      }

    (define/public (set-constraints mps)
      ; This method can only be used to transition from non-false to non-false
      (assert (and constraints mps))
      (if (equal? constraints mps)
          #f
          (let ([game-settings (multiplayer-settings-game-settings mps)])
            (set! constraints mps)
            (viewmodel->ui (multiplayer-settings-viewmodel mps))
            (update-fields mps)
            (respec)
            ;(println (list "client got:" game-settings))
            #t)))

    (define (update-fields mp-settings)
      (define-values (custom? handicap?)
        (let ([comp-type (if host-mode?
                             'host
                             (and mp-settings
                                  (multiplayer-settings-competition-type mp-settings)))])
          (case comp-type
            [(symmetric) (values #f #f)]
            [(handicap) (values #f #t)]
            [(custom) (values #t #t)]
            [(#f host) (values #t #f)]
            [else (fail "no matching case for:" comp-type)])))
      (define-syntax-rule (*enable [control enabled?] ...)
        (begin
          (and control (set-enabled control enabled?))
          ...))
      (*enable [@win-condition host-mode?]
               [@competition-type host-mode?]
               [@mode custom?]
               [@level custom?]
               [@time-attack-type custom?]
               [@blanks-enabled custom?]
               [@penalty-type custom?]
               [@handicap handicap?]
               [@custom-spec custom?])
      (void))

    (define/public (make-game-settings)
      (let* ([text (send @custom-spec get-value)]
             [port (open-input-string text)]
             [datum (read port)])
        (assoc->settings datum)))

    (define/public (make-mp-settings)
      (build-server-settings (get-value @win-condition)
                             (get-value @competition-type)
                             (make-game-settings)
                             (ui->viewmodel)))

    ; Define two conversion functions.
    ; 1) ui->viewmodel converts the state of the UI to a network-friendly format.
    ; 2) viewmodel->ui reverses the conversion.
    ; This might be brittle if the client and server are running different
    ; versions of the code, but it's fine for now.
    (define-syntax-rule (for-all-fields x)
      (x @win-condition
         @competition-type
         @mode
         @level
         @time-attack-type
         @blanks-enabled
         @penalty-type))
    (define (ui->viewmodel)
      (define-syntax-rule (make-assoc id ...)
        (list (cons 'id (get-value id))
              ...))
      (for-all-fields make-assoc))
    (define (viewmodel->ui vm)
      (define (setit control sym)
        (let ([result (assoc sym vm)])
          (when result
            (set-value control (cdr result)))))
      (define-syntax-rule (parse-assoc id ...)
        (begin (setit id 'id) ...))
      (for-all-fields parse-assoc))

    ; Before we exit, make sure everything is in the correct state
    (update-fields constraints)
    (respec)))

(module+ main
  (define frame (new frame%
                     [width 800]
                     [label "test"]))
  (define panel (new setup-panel%
                     [parent frame]
                     [host-mode? #t]
                     [start-game-callback (lambda args (println args))]))
  (send frame show #t))

(module+ test
  (require rackunit)

  (define (make-settings mode level)
    (let ([settings (build-settings mode level #f)])
      (struct-copy GameSettings settings
                   [misc:random-seed #f])))

  ; 2021-08-23 With this tuning, I was able to beat level 40 wave mode somewhat consistently.
  ; Going for large combos is essential.
  ; Then I added the `combos-require-fuel?` setting and turned it on and started losing.
  ; I think this was just a coincidence (I don't think I was relying on empty combos
  ; while I was winning), but I'm not sure.
  ; So we'll keep allowing empty combos unless it totally breaks the game.
  (let ([settings (make-settings 'wave 40)])
    (check-equal? (settings->assoc settings)
                  '((layout:mode . wave)
                    (layout:fuel-height . 10)
                    (layout:fuel-count . 30)
                    (layout:num-waves . 5)
                    (time-attack:type . #f)
                    (time-attack:seconds . 600)
                    (energy:initial . 10000)
                    (energy:max . 10000)
                    (energy:catalyst-cost . 250)
                    (energy:drain-rate . 5)
                    (energy:combo-payout . #(0 200 500 1000 2000))
                    (energy:fuel-value . 50)
                    (energy:horizontal-bonus-factor . 20)
                    (energy:wave-completion-bonus . 3000)
                    (penalty:type . #f)
                    (penalty:resistance . 1000)
                    (penalty:catalyst-cost . 0)
                    (penalty:floor-drop-amount . 4)
                    (attack:combo-payout . #(0 50 100 200 500 1000))
                    (attack:horizontal-bonus-factor . 50)
                    (misc:catalyst-deck . blank-12)
                    (misc:combos-require-fuel? . #f)
                    (misc:random-seed . #f)))))
