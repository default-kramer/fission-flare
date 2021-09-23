#lang racket/gui

(provide occupant-key->pict main-icon host-icon)

(require "util.rkt"
         pict)

; To ensure maximum cacheability, do not accept an occupant directly.
; Instead accept `key` which is a (listof (or/c symbol? number?)).
(define (occupant-key->pict key size #:darken? [darken? #f])
  (define (size* a [size size])
    (floor (* a size)))
  (define thickness
    (let ([val (max (size* 0.1) 4)])
      (if (odd? val) ; should be an even number, because we're going to halve it later
          (add1 val)
          val)))
  (define (joined-catalyst-pict color direction)
    ; We could build the pict for just one direction and `rotate` it,
    ; but that sometimes introduces one-pixel inaccuracies.
    ; I don't know why, so just do this instead:
    (let*-values ([(half orig-size)
                   (values (size* 0.5) size)]
                  [(pen brush)
                   (values (get-pen color thickness darken?)
                           (get-brush color darken?))]
                  [(r0 r1 r2 r3) ; radians in quarter-turn increments
                   (values 0 (/ pi 2) pi (+ pi (/ pi 2)))]
                  [(arc-size)
                   (values (- orig-size thickness))]
                  [(xmin xmax)
                   (values (/ thickness 2) (- orig-size (/ thickness 2)))]
                  [(ymin ymax)
                   (values xmin xmax)]
                  [(x0 y0
                       x1 y1
                       start-radians end-radians ccw?
                       x2 y2)
                   (case direction
                     [(u) (values xmin ymin
                                  xmin (- half thickness)
                                  r2 r0 #t
                                  xmax ymin)]
                     [(r) (values xmax ymin
                                  half ymin
                                  r1 r3 #t
                                  xmax ymax)]
                     [(d) (values xmax ymax
                                  xmax half
                                  r0 r2 #t
                                  xmin ymax)]
                     [(l) (values xmin ymin
                                  (- half thickness) ymin
                                  r1 r3 #f
                                  xmin ymax)]
                     [else (fail "unexpected direction" direction)])])
      (dc (lambda (dc dx dy)
            (define old-brush (send dc get-brush))
            (define old-pen (send dc get-pen))
            (send dc set-pen pen)
            (send dc set-brush brush)
            (define path (new dc-path%))
            (send path move-to x0 y0)
            (send path line-to x1 y1)
            (send path arc xmin ymin arc-size arc-size start-radians end-radians ccw?)
            (send path line-to x2 y2)
            (send path line-to x0 y0)
            (send path close)
            (send dc draw-path path dx dy)
            (send dc set-pen old-pen)
            (send dc set-brush old-brush))
          orig-size orig-size)))
  (define (single-catalyst-pict color)
    (let* ([pen (get-pen color thickness darken?)]
           [brush (get-brush color darken?)]
           [offset (/ thickness 2)]
           [size1 (max 0 (- size thickness))])
      (dc (lambda (dc dx dy)
            (define old-brush (send dc get-brush))
            (define old-pen (send dc get-pen))
            (send dc set-pen pen)
            (send dc set-brush brush)
            (send dc draw-ellipse (+ dx offset) (+ dy offset) size1 size1)
            (send dc set-pen old-pen)
            (send dc set-brush old-brush))
          size size)))
  (define (contaminant-pict color)
    (let* ([padding (size* 0.08)]
           [thickness (+ padding padding)]
           [size1 (- size thickness)]
           [half-size (size* 0.5)]
           [pen (get-pen color thickness darken?)]
           [brush (get-brush color darken? 'horizontal-hatch)])
      (dc (lambda (dc dx dy)
            (define old-brush (send dc get-brush))
            (define old-pen (send dc get-pen))
            (send dc set-pen pen)
            (send dc set-brush brush)
            (send dc draw-ellipse (+ dx padding) (+ dy padding) size1 size1)
            (send dc set-pen old-pen)
            (send dc set-brush old-brush))
          size size)))
  (define (fuel-pict color)
    ; Using pict (two `filled-ellipse` calls) is easier, but also introduces strange
    ; artifacts at certain sizes. I think it's because cc-superimpose introduces
    ; fractions attempting to perfectly center the ellipses.
    ; Instead, let's use `dc` and just accept the fact that we might be 1 pixel off
    ; center (when cell-size is odd).
    (let* ([size2 (size* 0.4)]
           [offset (quotient (- size size2) 2)]
           [brush (get-brush color darken?)])
      (dc (lambda (dc dx dy)
            (define old-brush (send dc get-brush))
            (define old-pen (send dc get-pen))
            (send dc set-pen black-pen)
            (send dc set-brush brush)
            (send dc draw-ellipse dx (+ dy offset) size size2)
            (send dc draw-ellipse (+ dx offset) dy size2 size)
            (send dc set-pen old-pen)
            (send dc set-brush old-brush))
          size size)))
  ; end helpers, begin main body
  (match key
    [(list 'mask o)
     (occupant-key->pict o size #:darken? #t)]
    [(list 'scale factor o)
     (cc-superimpose (blank size size)
                     (occupant-key->pict o (size* factor)))]
    [(list #f)
     (blank size)]
    [(list 'ground)
     (filled-rounded-rectangle size size #:color "gray")]
    [(list 'fuel color)
     (fuel-pict color)]
    [(list 'contaminant color)
     (contaminant-pict color)]
    [(list 'catalyst color #f)
     (single-catalyst-pict color)]
    [(list 'catalyst color direction)
     (joined-catalyst-pict color direction)]
    [else (fail "unexpected occupant key" key)]))

(define (get-pen c thickness darken?)
  (let* ([color (or (interpret-color c) white)]
         [color (if darken? (darken color) color)])
    (new pen% [width thickness] [color color])))

(define (get-brush c darken? [style 'solid])
  (let* ([color (interpret-color c)]
         [color (and color (if darken?
                               (darken color)
                               color))])
    (new brush%
         [color (or color transparent)]
         [style style])))

(define (interpret-color c)
  (case c
    [(r) red]
    [(y) yellow]
    [(b) blue]
    [(#f) #f]
    [else (fail "unexpected color" c)]))

(define (darken color)
  (make-color (quotient (send color red) 3)
              (quotient (send color green) 3)
              (quotient (send color blue) 3)))

(define red (make-color 255 0 0))
(define yellow (make-color 255 255 0))
(define blue (make-color 40 70 255))
(define white (make-color 255 255 255))
(define black (make-color 0 0 0))
(define transparent (make-color 0 0 0 0))

(define black-pen (new pen% [width 1] [color black]))


(define (make-icon key)
  (pict->bitmap (occupant-key->pict key 30)))

(define main-icon (make-icon '(fuel b)))
(define host-icon (make-icon '(fuel r)))
