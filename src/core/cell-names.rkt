#lang typed/racket

; defines and provides cell names, for example
; A1 = (make-loc 0 0)
; B8 = (make-loc 1 7)

(require (for-syntax racket/syntax)
         "data.rkt")

(define-for-syntax column-names "ABCDEFGH")
(define-for-syntax max-height 20)

(define-syntax (define-cell-ids stx)
  (syntax-case stx ()
    [(_ ctx #:x x)
     #`(begin
         #,@(for/list ([y (in-range max-height)])
              (let* ([x (syntax->datum #'x)]
                     [letter (string-ref column-names x)])
                (with-syntax ([id (format-id #'ctx "~a~a" letter (add1 y))])
                  #`(begin
                      (define id (make-loc #,x #,y))
                      (provide id))))))]
    [(_ ctx)
     #`(begin
         #,@(for/list ([x (in-range (string-length column-names))])
              #`(define-cell-ids ctx #:x #,x)))]))

(define-cell-ids in-this-context)
