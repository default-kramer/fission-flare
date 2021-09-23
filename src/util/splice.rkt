#lang typed/racket

(provide with-syntax/splice)

(define-syntax (with-syntax/splice stx)
  (syntax-case stx ()
    [(_ ([a b] ...) form)
     (eval-syntax #'(with-syntax ([a b] ...) #`form))]))

(module+ test
  (require typed/rackunit)

  (let ([x (with-syntax/splice ([(a b c) #'(x y z)])
             (quote (a b c)))])
    (check-equal? x '(x y z)))

  (define-for-syntax foo-fields
    #'([a : Integer]
       [b : Symbol]
       [c : String]))

  (define-values-for-syntax (foo-field-ids foo-field-types)
    (syntax-case foo-fields (:)
      [([id : Type] ...)
       (values #'(id ...)
               #'(Type ...))]))

  (with-syntax/splice ()
    (struct foo #,foo-fields #:transparent))

  (define the-foo (foo 42 'hello "world"))

  (define-type Assoc (Listof (Pairof Any Any)))

  (: foo->assoc (All (a) (case-> (-> foo Assoc)
                                 (-> (Listof a) (Listof a)))))
  (with-syntax/splice ([(field ...) foo-field-ids]
                       [ooo (quote-syntax ...)])
    (define (foo->assoc f)
      (match f
        [(foo field ...)
         `((field . ,field) ...)]
        ; just making sure that ooo works:
        [(list a b more ooo)
         (cons b (cons a more))])))

  (check-equal? (foo->assoc the-foo)
                '((a . 42) (b . hello) (c . "world")))
  (check-equal? (foo->assoc '(1 2 3 4 5))
                '(2 1 3 4 5))

  (: assoc-ref (-> Assoc Any Any))
  (define (assoc-ref lst key)
    (let ([pair (assoc key lst)])
      (if (pair? pair)
          (cdr pair)
          (error "Missing field" key))))

  (with-syntax/splice ([(field ...) foo-field-ids]
                       [(Type ...) foo-field-types]
                       [(pred ...) (generate-temporaries foo-field-types)])
    (begin
      (define pred (make-predicate Type))
      ...
      (define (assoc->foo [x : Assoc])
        (let ([field : Type
                     (let ([val (assoc-ref x 'field)])
                       (if (pred val)
                           val
                           (error "Type error" 'field "expected" 'Type "got" val)))]
              ...)
          (foo field ...)))))

  (let ([x (assoc->foo '((a . 1)
                         (c . "c")
                         (b . b)))])
    (check-equal? x (foo 1 'b "c"))))
