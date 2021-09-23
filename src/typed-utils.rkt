#lang typed/racket/base

(provide fail assert
         let*
         with-controller-type relax
         prop:authentic
         )

(require (submod "util/assert.rkt" typed)
         (submod "util/let++.rkt" typed)
         "util/with-controller-type.rkt"
         )

(require/typed racket [prop:authentic Struct-Type-Property])
