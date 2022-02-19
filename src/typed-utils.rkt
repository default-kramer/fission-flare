#lang typed/racket/base

(provide fail assert
         let*
         with-controller-type relax
         prop:authentic
         flatmap
         )

(require (submod "util/assert.rkt" typed)
         (submod "util/let++.rkt" typed)
         "util/with-controller-type.rkt"
         )

(require/typed racket [prop:authentic Struct-Type-Property])

(: flatmap (All (A B) (-> (-> A (Listof B)) (Listof A) (Listof B))))
(define (flatmap proc lst)
  (if (null? lst)
      null
      (append (proc (car lst))
              (flatmap proc (cdr lst)))))
