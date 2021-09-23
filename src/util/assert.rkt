#lang racket

(provide fail assert)

(require (for-syntax syntax/location))

(define-syntax (fail stx)
  (let* ([file (syntax-source-file-name stx)]
         [line (syntax-line stx)]
         [str (format "Failure on line ~a of ~a" line file)])
    (syntax-case stx ()
      [(_ x ...)
       (quasisyntax/loc stx
         (error #,str (list x ...)))])))

(define-syntax (assert stx)
  (syntax-case stx ()
    [(_ condition)
     (quasisyntax/loc stx
       (when (not condition)
         #,(syntax/loc stx
             (fail "Assert failed:" 'condition))))]))

; Just copy-paste the untyped code into a typed module.
; So far, no modifications have been needed.
(module typed typed/racket
  (provide fail assert)

  (require (for-syntax syntax/location))

  (define-syntax (fail stx)
    (let* ([file (syntax-source-file-name stx)]
           [line (syntax-line stx)]
           [str (format "Failure on line ~a of ~a" line file)])
      (syntax-case stx ()
        [(_ x ...)
         (quasisyntax/loc stx
           (error #,str (list x ...)))])))

  (define-syntax (assert stx)
    (syntax-case stx ()
      [(_ condition)
       (quasisyntax/loc stx
         (when (not condition)
           #,(syntax/loc stx
               (fail "Assert failed:" 'condition))))])))
