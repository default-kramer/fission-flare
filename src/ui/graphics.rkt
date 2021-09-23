#lang racket/gui

(provide frame->pict state->pict cell-size)

(require pict
         "util.rkt"
         "setup-panel.rkt"
         "occupant-picts.rkt"
         "../core.rkt")

; Using `scale` or `rotate` sometimes introduces unwanted artifacts.
; So instead, this parameter controls the desired pixel count (height and width)
; of a single cell:
(define cell-size (make-parameter 30))

; The viewmodel contains the state and all extra information that we need to draw.
; Commonly abbreviated `vm` in this file.
(struct viewmodel (state
                   ; Burst factor is used to scale the size of blanks being bursted.
                   ; Should be a number between 0 and 1 or #f
                   burst-factor
                   ; Fast Blinker is a boolean value that should be #t for a few frames
                   ; followed by #f for a few frames, repeating endlessly.
                   ; This can be used to control, for example, the energy gauge
                   ; to blink when it is critically low.
                   fast-blinker
                   ; extra-occs : a hash of (Loc -> Occ) mappings to be drawn as if they
                   ; were still on the grid. Used for the "ripple destruction" effect.
                   extra-occs
                   ; seconds-remaining : the number of seconds remaining for time attack or #f
                   seconds-remaining
                   ; frame-debug-str : any string
                   frame-debug-str
                   ) #:transparent)

(define black (make-color 0 0 0))
(define white (make-color 255 255 255))
(define red (make-color 255 0 0))
(define dark-gray (make-color 55 55 55))
(define transparent (make-color 0 0 0 0))
(define mask (make-color 0 0 0 0.7))
(define light-orange (make-color 255 180 60))
(define dark-orange (make-color 255 120 0))
(define green (make-color 30 255 30))

; Caching is very important.
; I don't know if freezing improves efficiency, but it seems to introduce
; anti-aliasing which looks better IMO.
; Update - but freezing looks horrible when Windows DPI is not 100%
(define occ->pict
  (let ([cache (make-hash)])
    (lambda (occupant-key)
      (let* ([size (cell-size)]
             [cache-key (cons `(#:size ,size) occupant-key)])
        (or (hash-ref cache cache-key #f)
            (let ([result (occupant-key->pict occupant-key size)]
                  #;[result (freeze result)])
              (hash-set! cache cache-key result)
              result))))))

(define (occupant-key vm occ)
  (cond
    [(not occ) '(#f)]
    [(ground? occ) '(ground)]
    [(fuel? occ) `(fuel ,(occupant-color occ))]
    [(catalyst? occ)
     (let* ([color (occupant-color occ)]
            [factor (and (not color)
                         (viewmodel-burst-factor vm))]
            [raw `(catalyst ,(occupant-color occ) ,(catalyst-direction occ))])
       (if factor
           `(scale ,factor ,raw)
           raw))]
    [(contaminant? occ) `(contaminant ,(occupant-color occ))]
    [else (fail "unexpected occupant" occ)]))

(define (occupant->pict vm occ)
  (occ->pict (occupant-key vm occ)))

(define (occupant->pict/maskable vm occ mask?)
  (let ([o (occupant-key vm occ)])
    (occ->pict (if mask?
                   (list 'mask o)
                   o))))

(define/contract (grid-pict vm)
  (-> viewmodel? pict?)
  (define state (viewmodel-state vm))
  (define top-line-height 10)
  (define width (state-width state))
  (define height (state-height state))
  (define preview-fall-state
    (car (state-apply state (list (action:plummet)))))
  (define (make-pict loc)
    (define occ (or (hash-ref (viewmodel-extra-occs vm) loc #f)
                    (state-get state loc)))
    (define preview-occ (and preview-fall-state
                             (not occ)
                             (state-get preview-fall-state loc)))
    (occupant->pict/maskable vm (or occ preview-occ) preview-occ))
  (define (make-row y)
    (apply hc-append (for/list ([x (in-range width)])
                       (make-pict (make-loc x y)))))
  (define grid-pict
    (let* ([picts (for/list ([y (in-range (sub1 height) -1 -1)])
                    (make-row y))]
           [width (pict-width (car picts))]
           [top-line (filled-rectangle width top-line-height #:color white #:draw-border? #f)]
           [picts (match picts
                    [(list a b more ...)
                     (list* a b top-line more)])]
           [grid-pict (apply vc-append picts)])
      grid-pict))
  (define (make-text str)
    (define t (text str (list white 'bold) (* 2 (cell-size))))
    (cc-superimpose
     (filled-rectangle (pict-width t) (pict-height t) #:color mask)
     t))
  (case (state-game-over? state)
    [(#f) grid-pict]
    [(win) (cc-superimpose grid-pict (make-text "Win"))]
    [(time-expired) (cc-superimpose grid-pict (make-text "Time"))]
    [else (cc-superimpose grid-pict (make-text "Lose"))]))

(define/contract (queue->pict vm queue)
  (-> viewmodel? (listof (cons/c occupant? occupant?)) pict?)
  (let* ([padding 20]
         [main-pict
          (apply vc-append
                 (for/list ([pair queue])
                   (vc-append (blank padding)
                              (hc-append (occupant->pict vm (car pair))
                                         (occupant->pict vm (cdr pair))))))]
         [w (+ (pict-width main-pict) (* 2 padding))]
         [h (+ (pict-height main-pict) padding)])
    (ct-superimpose
     (filled-rectangle w h #:color "gray" #:draw-border? #f)
     main-pict)))

(define (clock-pict2 val maxval [w 400] [h 80])
  (define (clamp x)
    (max 0 (min x 1)))
  (define text-size (floor (/ (* h 3) 4)))
  (define clock-ratio (clamp (1 . - . (val . / . maxval))))
  (define clock-color (if (val . > . 0) light-orange dark-orange))
  (lc-superimpose
   (filled-rectangle (* clock-ratio w) h #:color clock-color #:draw-border? #f)
   (rc-superimpose
    (blank w h)
    (text (~a val) '(bold) text-size))
   ; just for the border:
   (rectangle w h)))

(define (clock-pict state [w 400] [h 80])
  (let* ([settings (state-settings state)]
         [resistance (game-settings-penalty:resistance settings)]
         [ps (state-penalty-state state)]
         [countdown (penalty-state-countdown ps)])
    (clock-pict2 countdown resistance w h)))

(define (add-background pict color)
  (cc-superimpose (filled-rectangle (pict-width pict) (pict-height pict) #:color color)
                  pict))

(define (pad pict spec)
  (define-values (top right bottom left)
    (match spec
      [(list a b c d)
       (values a b c d)]
      [x (values x x x x)]))
  (let ([w (inexact->exact (ceiling (+ left right (pict-width pict))))]
        [h (inexact->exact (ceiling (+ top bottom (pict-height pict))))])
    (lt-superimpose (blank w h)
                    (translate pict left top))))

(define (battery-pict percent [blink? #f])
  (define size (cell-size))
  (define (size* a [size size])
    (floor (* a size)))
  (let* ([thickness (size* 0.25)]
         [w (size* 3)]
         [h (* w 2)]
         [knob-w size]
         [knob-h (size* 0.3)]
         [knob-x (/ (- w knob-w) 2)]
         [bar-width (- w thickness thickness (/ w 5))]
         [bar-x-offset (/ (- w bar-width) 2)]
         [bar-count 5]
         [bar-h (floor (/ h (* bar-count 1.5)))]
         [bar-y-offset (+ bar-h thickness)])
    (dc (lambda (dc dx dy)
          (define old-pen (send dc get-pen))
          (define old-brush (send dc get-brush))
          (send dc set-pen (new pen% [width thickness] [color black]))
          (send dc draw-rounded-rectangle (+ dx thickness) (+ dy thickness knob-h) w h)
          (send dc set-brush (new brush% [color black]))
          (send dc draw-rounded-rectangle (+ dx thickness knob-x) (+ dy thickness) knob-w knob-h)

          (when (not blink?)
            (let ([color (if (< percent 0.4)
                             light-orange
                             green)])
              (send dc set-pen (new pen% [width 1] [color color]))
              (send dc set-brush (new brush% [color color]))
              (for ([i (in-range bar-count)])
                (let* ([percent-per-bar (/ 1 bar-count)]
                       [range-bottom (- 1 (* (add1 i) percent-per-bar))]
                       [excess (- percent range-bottom)]
                       [foo (min 1 (max 0 (/ excess percent-per-bar)))]
                       [bar-h-adjusted (* foo bar-h)]
                       [bar-h-missing (- bar-h bar-h-adjusted)])
                  (send dc draw-rectangle
                        (+ dx thickness bar-x-offset)
                        (+ dy bar-h-missing (* (add1 i) bar-y-offset))
                        bar-width
                        bar-h-adjusted)))))

          (send dc set-pen old-pen)
          (send dc set-brush old-brush))
        (+ w thickness thickness)
        (+ h thickness thickness knob-h))))

(define (gauge-pict energy max-energy blinker)
  (let* ([percent (/ energy max-energy)]
         [critical-level 0.15]
         [blink? (and blinker (<= percent critical-level))]
         [battery (battery-pict (max 0.1 percent) blink?)]
         [size (cell-size)]
         [label (text (format "~a kW" energy) '(bold) size)])
    (vr-append battery label)))

(define (seconds->string total-seconds)
  (define (str num [padding #f])
    (let ([num (inexact->exact (truncate num))])
      (if padding
          (~a num #:min-width padding #:left-pad-string "0" #:align 'right)
          (~a num))))
  (let*-values ([(seconds)
                 (truncate total-seconds)]
                [(fraction)
                 (- total-seconds seconds)]
                [(minutes seconds)
                 (quotient/remainder seconds 60)])
    (format "~a:~a.~a"
            (str minutes)
            (str seconds 2)
            (str (* fraction 100) 2))))

(define (time-attack-pict energy seconds-remaining [blinker? #f])
  (let* ([size (cell-size)]
         [color (if (< seconds-remaining 20)
                    red
                    black)]
         [color (if (and blinker? (< seconds-remaining 10))
                    transparent
                    color)]
         [color (if (<= seconds-remaining 0)
                    black
                    color)])
    (vr-append
     (text (seconds->string seconds-remaining)
           (list 'bold color)
           size)
     (blank 10 10)
     (text (format "~a kW" energy) '(bold) size))))

(define (combo-pict explanations size)
  (match explanations
    [(list (list heading number detail) more ...)
     (vr-append
      (text (format "~a ~a" heading number) '() size)
      (text detail '() (- size 2))
      (combo-pict more size))]
    [(list)
     (blank 0 0)]))

(define (stats-pict state)
  (let* ([stats (state-stats state)]
         [spawn-count (stats-spawn-count stats)]
         [spawn-energy (stats-spawn-energy stats)]
         [waiting-frames (stats-waiting-frames stats)]
         [waiting-energy (stats-waiting-energy stats)]
         [size 12]
         [spacer (blank (* size 11) size)]
         [txt (lambda (str . args)
                (text (apply format (cons str args)) '() size))]
         [current-combo (state-current-combo state)]
         [combo (or current-combo (state-previous-combo state))]
         [combo-name (if current-combo "Current" "Previous")]
         [explanations (if combo
                           (combo-explanations combo)
                           (list))])
    (vr-append
     (txt "catalysts: ~a" (stats-spawn-count stats))
     (txt "energy cost: ~a" (stats-spawn-energy stats))
     spacer
     (txt "waiting frames: ~a" (stats-waiting-frames stats))
     (txt "energy cost: ~a" (stats-waiting-energy stats))
     spacer
     (txt "~a Combo:" combo-name)
     (combo-pict explanations size)
     )))

(define (viewmodel->pict vm show-clock? show-queue?)
  (define state (viewmodel-state vm))
  (define pgrid
    (let ([main (add-background (grid-pict vm) black)]
          [str (viewmodel-frame-debug-str vm)])
      (if str
          (lt-superimpose main
                          (text str (list white) 12))
          main)))
  (define pleft
    (if show-clock?
        (let ([w (pict-width pgrid)]
              [h (* (cell-size) 2)])
          (vc-append pgrid
                     (clock-pict state w h)))
        pgrid))
  (define pmain
    (if show-queue?
        (let ([spawns (state-next-spawns state 5)]
              [vm (struct-copy viewmodel vm
                               [burst-factor #f])])
          (ht-append pleft
                     (pad (queue->pict vm spawns) '(0 20 20 20))))
        pleft))
  (let* ([energy (state-energy state)]
         [max-energy (state-max-energy state)]
         [blinker (viewmodel-fast-blinker vm)]
         [seconds-remaining (viewmodel-seconds-remaining vm)]
         [left (stats-pict state)]
         [left (cond
                 [seconds-remaining
                  (vr-append (pad (time-attack-pict energy seconds-remaining blinker) '(0 0 35 0))
                             left)]
                 [max-energy
                  (vr-append (pad (gauge-pict energy max-energy blinker) '(0 0 35 0))
                             left)]
                 [else left])]
         [left (pad left '(0 12 0 0))])
    (ht-append left pmain)))

(define (state->pict state [show-clock? #t] [show-queue? #t])
  (let* ([burst-factor #f]
         [fast-blinker #f]
         [extra-occs (hash)]
         [vm (viewmodel state burst-factor fast-blinker extra-occs #f #f)])
    (viewmodel->pict vm show-clock? show-queue?)))

(define (frame->pict frame)
  (let* ([state (frame-state frame)]
         [info (frame-info frame)]
         [kind (cadr info)]
         [timing (frame-timing frame)]
         [frames-needed (timing-bursting timing)]
         [counter (frame-counter frame)]
         [frames-elapsed (- counter (car info))]
         [burst-delay 6] ; how many frames before we start shrinking the blanks
         [burst-factor (and (equal? 'bursting kind)
                            (> frames-elapsed burst-delay)
                            (- 1 (/ (- frames-elapsed burst-delay)
                                    (- frames-needed burst-delay))))]
         [blink-rate 3]
         [fast-blinker (= 0 (modulo (quotient counter blink-rate) 2))]
         [extra-occs (frame-extra-occs frame)]
         [time-remaining (frame-time-remaining frame)]
         [time-remaining (and time-remaining (max 0 time-remaining))]
         [debug-str (format "~a ~a" kind counter)]
         [vm (viewmodel state burst-factor fast-blinker extra-occs time-remaining debug-str)])
    (viewmodel->pict vm #t #t)))

; Just for testing / debugging
(define (build-picts)
  (flatten (list (occ->pict '(#f))
                 (occ->pict '(ground))
                 (for/list ([color '(r y b)])
                   (occ->pict `(fuel ,color)))
                 (for/list ([color '(r y b)])
                   (occ->pict `(contaminant ,color)))
                 (for/list ([color '(r y b #f)]
                            #:when #t
                            [dir '(#f u r d l)])
                   (occ->pict `(catalyst ,color ,dir))))))

(module+ test
  ; make sure no error is thrown at any size, including 0
  (for ([i (in-range 30)])
    (parameterize ([cell-size i])
      (void (build-picts)))))

(module+ main
  (define (make-state level)
    (let ([settings (build-settings 'standard level #f)])
      (make-initial-state settings)))
  (clock-pict2 1300 700)
  (clock-pict2 -100 800)
  (state->pict (make-state 10))

  ; The following is useful for debugging drawing problems.
  ; Also try changing the border to black instead of the occupant's color.
  (parameterize ([cell-size 100])
    (for/list ([dir '(#f u r d l)])
      (cc-superimpose (filled-rectangle (cell-size) (cell-size) #:color "green" #:draw-border? #f)
                      (occ->pict `(catalyst r ,dir)))))
  (flatten (build-picts))
  (ht-append (occ->pict '(catalyst b r))
             (occ->pict '(catalyst r l)))
  )
