#lang typed/racket

(require "../typed-utils.rkt"
         "data.rkt"
         "grid.rkt"
         (for-syntax syntax/parse)
         racket/match)

; The code in this file is not wired up to anything yet.
; Ideally we could programmatically generate interesting puzzles.
; But even if we just hard-code N puzzles, using mirror images might be enough
; to "fool the player" into thinking we have N*2 puzzles.
; Using mirror images and color swaps might bring us up to N*2*6 puzzles.

(struct puzzle ([grid : Grid]
                [queue : (Listof (Pairof Catalyst Catalyst))])
  #:transparent)

(: make-puzzle (-> (Listof (Listof Symbol)) (Listof Symbol) puzzle))
(define (make-puzzle pattern queue)
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
  (puzzle (parse-grid pattern) (parse-queue queue)))

(define-syntax (parse-puzzles stx)
  (syntax-parse stx
    [(_ (name:expr pattern:expr ...
                   #:queue [q-occ:expr ...]
                   #:solution todo:expr ...)
        ...)
     #'(list (make-puzzle '(pattern ...)
                          '[q-occ ...])
             ...)]))

(define puzzles
  {parse-puzzles
   ("Horizontal delay"
    [.. RR ..]
    [.. RR ..]
    [.. RR ..]
    [YY YY YY]
    #:queue [<o y> <b r>]
    #:solution "obvious")

   ("Horizontal setup"
    [BB .. .. ..]
    [BB .. .. ..]
    [BB .. .. ..]
    [YY .. .. YY]
    #:queue [<b y> <y b>]
    #:solution "obvious")

   ("Tree"
    [YY .. BB]
    #:queue [<y b> <b y> <y b>]
    #:solution "obvious")

   ("Prioritize"
    [RR .. BB]
    [RR YY BB]
    [.. YY ..]
    [.. YY ..]
    #:queue [<o y> <b b> <r r>]
    #:solution "obvious")

   ("Prioritize 2"
    [.. BB]
    [.. BB]
    [.. BB]
    [BB RR]
    [BB RR]
    #:queue [<o b> <b b> <r r>]
    #:solution "obvious")

   ("X-ray 101"
    [.. RR]
    [.. RR]
    [.. RR]
    [BB YY]
    [BB YY]
    #:queue [<o r> <y b> <b y>]
    #:solution "obvious")

   ("Horizontal X-ray"
    [.. BB .. ..]
    [RR BB .. ..]
    [RR BB .. ..]
    [YY .. .. YY]
    #:queue [<o r> <b y> <r y>]
    #:solution
    [<r y> .. ..]
    [.. <b y> ..]
    [<r o> .. ..]
    [.. BB .. ..]
    [RR BB .. ..]
    [RR BB .. ..]
    [YY .. .. YY])

   ("Platform"
    [.. RR ..]
    [.. RR ..]
    [.. BB BB]
    [.. .. ..]
    #:queue [<o r> <r r> <b b>]
    #:solution
    [.. .. r^ ..]
    [<b b> r_ ..]
    [.. <r o> ..]
    [.. .. RR ..]
    [.. .. RR ..]
    [.. .. BB BB]
    [.. .. .. ..])

   ("Platform X-ray"
    [.. .. .. RR ..]
    [.. .. .. RR ..]
    [.. .. .. RR YY]
    [RR BB .. BB YY]
    [RR .. .. .. ..]
    #:queue [<o y> <r y> <y b> <r b> <r r>]
    #:solution
    [.. .. .. .. b^]
    [.. .. .. .. r_]
    [.. .. .. <r y>]
    [.. .. .. <o y>]
    [.. .. .. RR ..]
    [r^ .. .. RR ..]
    [r_ .. .. RR YY]
    [RR BB b^ BB YY]
    [RR .. y_ .. ..])

   ("Savvy"
    [BB .. .. ..]
    [.. BB RR YY]
    [.. BB RR YY]
    [.. BB .. ..]
    #:queue [<b b> <r y> <y r> <r b>]
    #:solution
    [.. .. <r y>]
    [.. r^ <r y>]
    [BB b_ <b b>]
    [.. BB RR YY]
    [.. BB RR YY]
    [.. BB .. ..])

   ("Savvy 2"
    [BB .. .. .. BB]
    [.. RR .. YY ..]
    [.. RR .. YY ..]
    #:queue [<o b> <b b> <y b> <r r> <y b>]
    #:solution
    [.. .. .. <y b>]
    [.. r^ .. <y b>]
    [.. r_ .. b^ ..]
    [BB <b b> o_ BB]
    [.. RR .. YY ..]
    [.. RR .. YY ..])

   ("Bridge?"
    [YY .. YY YY .. YY]
    #:queue [<o r> <y b> <y r>]
    #:solution
    [.. b^ .. .. .. ..]
    [.. y_ .. .. .. ..]
    [.. <o r> .. r^ ..]
    [YY .. YY YY y_ YY])

   (""
    [<b r> ..]
    [<b r> RR]
    [.. RR ..]
    [BB YY YY]
    #:queue [<o r> <b y> <r r> <r y>]
    #:solution
    [y^ r^ <r y>]
    [b_ o_ r^ ..]
    [<b r> r_ ..]
    [<b r> RR ..]
    [.. RR .. ..]
    [BB YY YY ..])

   ("Tower 1"
    [.. YY ..]
    [RR YY YY]
    [RR YY ..]
    [RR BB ..]
    [.. BB ..]
    #:queue [<o y> <y y> <y b> <b r>]
    #:solution
    [<r b> ..]
    [.. <b y>]
    [.. <y y>]
    [.. <o y>]
    [.. YY ..]
    [RR YY YY]
    [RR YY ..]
    [RR BB ..]
    [.. BB ..])

   ("Tower 2"
    [.. YY ..]
    [.. YY YY]
    [RR YY YY]
    [RR BB ..]
    [.. BB ..]
    #:queue [<o r> <y y> <y b> <b r>]
    #:solution
    [<r b> ..]
    [.. <b y>]
    [.. <y y>]
    [<r o> ..]
    [.. YY ..]
    [.. YY YY]
    [RR YY YY]
    [RR BB ..]
    [.. BB ..])

   ("Difficult!"
    [.. .. BB BB]
    [.. .. YY ..]
    [RR BB .. ..]
    #:queue [<y b> <y y> <r o> <b b> <y b> <r y> <b r>]
    #:solution
    [b^ .. b^ ..]
    [r_ .. y_ ..]
    [<r y> <y y>]
    [<b b> <y b>]
    [o^ .. BB BB]
    [r_ .. YY ..]
    [RR BB .. ..])
   })
