#lang typed/racket

(provide with-controller-type relax)

(require (for-syntax syntax/parse)
         racket/stxparam
         racket/splicing)

; Typed Racket struggles with types like
#;(case-> (-> 'get (-> Integer))
          (-> 'set (-> Integer Void)))
; If you try to provide this to untyped code, you get
;    Type Checker: could not convert type to a contract;
;    function type has two cases of arity 1
; And if you try to provide it as Any, you get a runtime error
;    contract violation
;    Attempted to use a higher-order value passed as `Any` in untyped code: #<procedure>
; But there is a way. The following "relaxed type" seems to be the easiest and
; safest way to provide the procedure to untyped code:
#;(-> (U 'get 'set) (U (-> Integer)
                       (-> Integer Void)))
; This macro makes it easy to convert from the original type to the relaxed type.

(define-syntax-parameter relax
  (lambda (stx) (raise-syntax-error 'relax "Used out of context" stx)))

(define-syntax (with-controller-type stx)
  (syntax-parse stx
    [(_ TypeName:id ([sym:id function-type:expr]
                     ...)
        body ...)
     (with-syntax ([ooo (quote-syntax ...)])
       (syntax/loc stx
         (begin
           (define-type TypeName (case-> (-> 'sym function-type)
                                         ...))
           (: relax-proc (-> TypeName (-> (U 'sym ...)
                                          (U function-type ...))))
           (define (relax-proc it)
             (lambda (s)
               (let ([sym (it 'sym)]
                     ...)
                 (case s
                   [(sym) sym]
                   ...))))
           (splicing-syntax-parameterize ([relax (lambda (stx)
                                                   (syntax-case stx ()
                                                     [(_ stuff ooo)
                                                      (syntax/loc stx
                                                        (relax-proc stuff ooo))]
                                                     [id
                                                      (syntax/loc stx relax-proc)]))])
             body ...))))]))

(module+ test
  (require typed/rackunit)

  (with-controller-type IntBox ([get (-> Integer)]
                                [inc (-> Integer)]
                                [set (-> Integer Void)])
    (: intbox (-> Integer IntBox))
    (define (intbox i)
      (lambda (sym)
        (case sym
          [(get)
           (lambda () i)]
          [(inc)
           (lambda ()
             (set! i (add1 i))
             i)]
          [(set)
           (lambda (new-i)
             (set! i new-i))])))

    (define (relaxed-intbox [i : Integer])
      (relax (intbox i))))

  (let ([x (intbox 1)])
    (check-equal? 1 ((x 'get)))
    (check-equal? 2 ((x 'inc)))
    ((x 'set) 42)
    (check-equal? 42 ((x 'get)))
    (check-equal? 43 ((x 'inc))))

  ; Make sure our relaxed version has the expected type
  (void (ann relaxed-intbox (-> Integer (-> (U 'get 'inc 'set)
                                            (U (-> Integer)
                                               (-> Integer Void)))))))
