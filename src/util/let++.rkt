#lang racket

(provide (rename-out [let++ let*]))

(define-syntax (let++ stx)
  (syntax-case stx ()
    [(let++ (#:break a
             more ...)
            body ...)
     (syntax-case #'a (when)
       [(when condition x y ...)
        (syntax/loc #'a
          (if condition
              (begin x y ...)
              (let++ (more ...)
                     body ...)))]
       [else
        (raise-syntax-error #f "#:break must be followed by a `when` expression"
                            stx #'a)])]
    [(let++ (a more ...)
            body ...)
     (syntax-case #'a ()
       ([(id ...) stuff ...]
        (syntax/loc #'a
          (let-values (a)
            (let++ (more ...)
                   body ...))))
       ([id stuff ...]
        (syntax/loc #'a
          (let (a)
            (let++ (more ...)
                   body ...)))))]
    [(let++ () body ...)
     (syntax/loc stx
       (let () body ...))]))

; Just copy-paste the untyped code into a typed module.
; So far, no modifications have been needed.
(module typed typed/racket
  (provide (rename-out [let++ let*]))

  (define-syntax (let++ stx)
    (syntax-case stx ()
      [(let++ (#:break a
               more ...)
              body ...)
       (syntax-case #'a (when)
         [(when condition x y ...)
          (syntax/loc #'a
            (if condition
                (begin x y ...)
                (let++ (more ...)
                       body ...)))]
         [else
          (raise-syntax-error #f "#:break must be followed by a `when` expression"
                              stx #'a)])]
      [(let++ (a more ...)
              body ...)
       (syntax-case #'a ()
         ([(id ...) stuff ...]
          (syntax/loc #'a
            (let-values (a)
              (let++ (more ...)
                     body ...))))
         ([id stuff ...]
          (syntax/loc #'a
            (let (a)
              (let++ (more ...)
                     body ...)))))]
      [(let++ () body ...)
       (syntax/loc stx
         (let () body ...))])))
