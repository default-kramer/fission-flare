#lang typed/racket

(provide write-csv-line)

(require (only-in "../ai.rkt"
                  Orientation ori-label)
         racket/fixnum
         "../../typed-utils.rkt"
         "../data.rkt"
         "../grid.rkt")

(: write-csv-line (-> Output-Port State Orientation Void))
(define (write-csv-line port state ori)
  ; For now only handle normal kinds (no doubles, no blanks)
  (let ([kind (catalyst-kind state)])
    (when (equal? "normal" kind)
      (display (catalyst-kind state) port)
      (display "," port)
      (display (ori-label ori) port)
      (display "," port)
      (write-grid-features port state)
      (displayln "" port)))
  (void))

(define (catalyst-colors [state : State])
  (let* ([mover (state-mover state)]
         #:break (when (not mover)
                   (fail "state has no mover")))
    (list (catalyst-color (mover-occ-a mover))
          (catalyst-color (mover-occ-b mover)))))

(define (catalyst-kind [state : State])
  (let ([colors (catalyst-colors state)])
    (match colors
      [(list a a)
       "double"]
      [(list a #f)
       "halfblank"]
      [(list a b)
       "normal"]
      [else
       (fail "unexpected mover colors" colors)])))

(define (write-grid-features [port : Output-Port] [state : State])
  (let* ([grid (state-grid state)]
         [w (grid-width grid)]
         [h (grid-height grid)]
         ; TODO the grid has 2 empty top rows, should redo this
         ; for grep: mover-reserved-rows
         [h (fx- h 2)]
         [colors (catalyst-colors state)]
         ; From here on out, this function is assuming a normal catalyst
         #:break (when (not (equal? "normal" (catalyst-kind state)))
                   (fail "TODO need to support other catalyst kinds"))
         [c1 (first colors)]
         [c2 (second colors)]
         [c3 (remove c1 (remove c2 '(r y b)))])
    (: do-it (-> (Listof Boolean) Any))
    (define (do-it flags)
      (if (empty? flags)
          (void)
          (begin (display (if (car flags) 1 0) port)
                 (do-it (cdr flags)))))
    (for ([x (in-range w)])
      (for ([y (in-range h)])
        (let* ([occ (grid-get grid (make-loc x y))]
               [color (and occ (occupant-color occ))]
               [dir : (U 'none 'u 'd 'l 'r)
                    (if (catalyst? occ)
                        ; #f means no partner, which we can make the same as
                        ; up or down via the vertical divorce principle
                        (or (catalyst-direction occ) 'u)
                        'none)])
          (do-it (list (equal? color c1)
                       (equal? color c2)
                       (equal? color c3)
                       (and occ (not color)) ; blank
                       (not occ) ; vacant
                       (fuel? occ)
                       (or (equal? 'd dir) (equal? 'u dir)) ; loose catalyst
                       (equal? 'l dir)
                       (equal? 'r dir)
                       ; I planned a "stable?" feature here, but skip it for now
                       )))))
    (void)))
