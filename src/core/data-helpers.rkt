#lang typed/racket

; Avoid cluttering up data.rkt by moving the helpers into this file.
; This should only be required by data.rkt
(provide ss define-deserializer write-dto read-datum define-assoc-converters)

(require typed/racket/unsafe
         "../typed-utils.rkt"
         )

(require/typed racket
               [call-with-default-reading-parameterization (-> (-> Any) Any)]
               )

(unsafe-require/typed racket
                      ; https://github.com/racket/typed-racket/issues/1131
                      [vector->immutable-vector (All (a) (case-> (-> (Vectorof a) (Immutable-Vectorof a))
                                                                 (-> VectorTop VectorTop)))])

; Writes a DTO as an S-expression, because Racket's built-in printer is too hard
; to control. (If you don't think so wait until hashes start printing as
; #hash(...) for reasons unknown but probably related the fact that a fuel inside
; the hash got chaperoned somewhere along the way.)
(: write-dto (-> Any Output-Port Void))
(define (write-dto dto port)
  (define (put str)
    (display str port))
  (: go (-> Any Any))
  (define (go dto)
    (cond
      [(list? dto)
       (begin
         (put " (L") ; L = list
         (map go dto)
         (put ")"))]
      [(pair? dto)
       (begin
         (put " (C") ; C = cons
         (go (car dto))
         (go (cdr dto))
         (put ")"))]
      [(hash? dto)
       (begin
         (put " (H") ; H = hash
         (for ([kv (hash->list dto)])
           (go (car kv))
           (go (cdr kv)))
         (put ")"))]
      [(vector? dto)
       (begin
         (put " (V") ; V = vector
         (for ([i (in-range (vector-length dto))])
           (go (vector-ref dto i)))
         (put ")"))]
      [(struct? dto)
       (let* ([vec (struct->vector dto)]
              [sym (vector-ref vec 0)]
              ; transform 'struct:grid to "grid"
              [name (string-replace (format "~a" sym) "struct:" "")])
         (put " (")
         (put name)
         (for ([i (in-range 1 (vector-length vec))])
           (go (vector-ref vec i)))
         (put ")"))]
      [(or (symbol? dto)
           (keyword? dto)
           (string? dto)
           (number? dto)
           (boolean? dto)
           (string? dto))
       (begin
         (put " ")
         (print dto port))]
      [else
       (fail "Cannot serialize" dto)]))
  (go dto)
  ; Without a trailing space, the receiver could block waiting for more.
  ; When it gets "42 " it can be sure that the datum is complete.
  (put " ")
  (flush-output port)
  (void))

(: read-datum (-> Input-Port Any))
(define (read-datum in)
  (call-with-default-reading-parameterization
   (lambda ()
     (parameterize ([read-accept-box #f]
                    [read-accept-compiled #f]
                    [read-accept-graph #f]
                    [read-accept-quasiquote #f]
                    [read-accept-reader #f]
                    [read-accept-lang #f])
       (read in)))))

(: try-make-immutable (-> Any Any))
(define (try-make-immutable x)
  (cond
    [(vector? x)
     (vector->immutable-vector x)]
    [else x]))

(define-for-syntax (format-stx id pattern [intern? #t])
  (let* ([str (format pattern (syntax-e id))]
         [proc (if intern? string->symbol string->uninterned-symbol)]
         [sym (proc str)])
    (datum->syntax id sym (and intern? id) (and intern? id))))

; "ss" = standard struct
;
; The `struct-id*` alias is useful for at least two things
; 1: (struct-copy state* state <blah>...)
; 2: (state* #:deserializer) will expand to a deserialization procedure
;    having type (-> (Listof Any) State)
(define-syntax (ss stx)
  (syntax-case stx ()
    [(_ TypeId struct-id ([field : Type] ...))
     (with-syntax ([::: (quote-syntax :)] ; because `:` matches the caller's
                   [ooo (quote-syntax ...)]
                   [make-id (format-stx #'struct-id "make-~a")]
                   [struct-id* (format-stx #'struct-id "~a*")]
                   [(deserialize-id) (generate-temporaries #'(struct-id))]
                   [(pred-id ...) (generate-temporaries #'(field ...))])
       (syntax/loc stx
         (begin
           ; Must use the original `id` here so that we can provide it to untyped Racket:
           (struct struct-id ([field : Type] ...)
             ; There can be a 20x slowdown if a grid gets chaperoned!
             ; Use prop:authentic to fail fast.
             #:property prop:authentic #t
             #:type-name TypeId
             #:extra-constructor-name make-id
             #:transparent)
           (define-syntax struct-id*
             (impersonate-procedure
              (syntax-local-value #'struct-id)
              (lambda (stx)
                (values (lambda (stx)
                          (syntax-case stx ()
                            [(_ #:deserializer)
                             (syntax/loc stx deserialize-id)]
                            [else stx]))
                        stx))))
           (define pred-id (make-predicate Type))
           ...
           (::: deserialize-id (-> (Listof Any) TypeId))
           (define (deserialize-id lst)
             (match lst
               [(list field ...)
                #:when (and (pred-id field)
                            ...)
                (struct-id field ...)]
               #;[(list field ...)
                  (begin
                    (println "DEBUGGING")
                    (println (list 'field 'Type (pred-id field) (immutable? field) field))
                    ...
                    (fail "[Debugging] Cannot deserialize:" struct-id lst))]
               [else
                (fail "Cannot deserialize:" struct-id lst)]))
           )))]))

(define-syntax-rule (satisfy field-id pred-id)
  (let* ([item field-id]
         [item2 (try-make-immutable item)])
    (cond
      [(pred-id item) item]
      [(pred-id item2) item2]
      [else (fail "Field:" 'field-id
                  "Expected:" pred-id
                  "Got:" item)])))

(define-syntax-rule (define-assoc-converters ooo StructType struct-id
                      struct->assoc assoc->struct
                      ([field-id pred-id] ...))
  (begin
    (: struct->assoc (-> StructType Any))
    (define (struct->assoc it)
      (match it
        [(struct-id field-id ...)
         `((field-id . ,field-id) ...)]))
    (: assoc->struct (-> Any StructType))
    (define (assoc->struct x)
      (match x
        [(list (cons 'field-id field-id) ...)
         (let ([field-id (satisfy field-id pred-id)]
               ...)
           (struct-id field-id ...))]
        [else
         (raise-argument-error
          'assoc->struct
          (format "An assoc like ~s" '((field-id . <val>) ...))
          x)]))))

(module builtin-deserializers racket/base
  (provide (all-defined-out))
  (define (to-list lst) lst)
  (define (to-hash lst) (apply hash lst))
  (define (to-cons lst) (apply cons lst))
  (define (to-vector lst) (apply vector-immutable lst)))

(unsafe-require/typed 'builtin-deserializers
                      [to-list (-> (Listof Any) Any)]
                      [to-hash (-> (Listof Any) Any)]
                      [to-cons (-> (Listof Any) Any)]
                      [to-vector (-> (Listof Any) Any)])

(: handle-quote (-> (Listof Any) Any))
(define (handle-quote args)
  (define arg (match args
                [(list a) a]
                [else (fail "multiple values were quoted?" args)]))
  (cond
    [(vector? arg)
     (to-vector (vector->list arg))]
    [(symbol? arg)
     arg]
    [else
     (fail "Cannot deserialize, unexpectedly quoted" arg)]))

;;; define-deserializer ;;;
; In the current module, defines `datum->dto` as a procedure which takes a datum and
; returns an instance of one of the `id`s or a primitive like list, hash, etc.
;
; In the test submodule, defines `the` such that `(the id)` expands to
; `(id arg ...)`. Also tests that each `(the id)` is equal? to itself
; following a serialization round trip.
(define-syntax (define-deserializer stx)
  (syntax-case stx ()
    [(_ datum->dto the (id arg ...) ...)
     (with-syntax ([(id* ...) (map (lambda (id) (format-stx id "~a*"))
                                   (syntax->list #'(id ...)))])
       (syntax/loc stx
         (begin
           (: datum->dto (-> Any Any))
           (define (datum->dto val)
             (if (list? val)
                 (let ([deserializer
                        (case (car val)
                          ; Special built-in cases:
                          [(quote) #f]
                          [(V) to-vector]
                          [(L) to-list]
                          [(H) to-hash]
                          [(C) to-cons]
                          [(id) (id* #:deserializer)]
                          ...
                          [else (fail "cannot deserialize:" val)])])
                   (cond
                     [deserializer
                      (deserializer (map datum->dto (cdr val)))]
                     [else
                      (handle-quote (cdr val))]))
                 val))

           (module+ test
             (require typed/rackunit)

             (define (dto->string val)
               (define os (open-output-string))
               (write-dto val os)
               (get-output-string os))
             (define (string->dto [str : String])
               (define datum (read-datum (open-input-string str)))
               (datum->dto datum))

             (define (round-trip x)
               (string->dto (dto->string x)))

             (define-syntax (the stx)
               (syntax-case stx (id ...)
                 [(_ id)
                  (syntax/loc #'id
                    (id arg ...))]
                 ...))
             (let* ([thing (the id)]
                    [thing2 (round-trip thing)])
               ;(println (list "testing round trip:" thing))
               ; for some reason, check-equal? tries to chaperone
               ;(check-equal? (ann thing2 Any) (ann thing Any))
               (check-true (equal? thing2 thing)))
             ...
             ))))]))
