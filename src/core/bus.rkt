#lang typed/racket

(provide Busof make-bus bus-dequeue bus-enqueue! bus-discard-all)

; A bus is basically a queue, with the following semantics:
; * The dequeue operation is pure -- it returns the value of the current node
;   and a new bus pointing to the next node.
; * The enqueue! operation is effectful -- all other buses that share the
;   same underlying list will be able to reach the newly-added item.
; In retrospect, this seems like a weird design - perhaps I should have provided
; separate reader and writer APIs... but whatever, it works.
; Just remember that if you are holding a reference to the start of the list,
; you are preventing the entire list from being GC'd. (This app probably
; doesn't have any buses that live long enough for that to be a problem.)
;
; This module uses `box-cas!` to achieve thread-safety.

(require "../typed-utils.rkt")

; Create the `node` and `fnode` types, where `fnode` means "first node".
; Our linked lists will be constructed like this (boxes omitted):
#;((fnode #f) ; the empty list
   ; now if we add one item we get:
   (fnode (node "one" #f))
   ; add another item:
   (fnode (node "one" (node "two" #f)))
   ; and so on ...
   )

; Once the `next` box contains a node it becomes immutable.
; Note that the box does not accept an `fnode`; it must be a `node`.
(struct (A) fnode ([next : (Boxof (U #f (node A)))])
  #:property prop:authentic #t)

(struct (A) node fnode ([item : A])
  #:property prop:authentic #t)

; The `latest` node is not guaranteed to be the end of the list, but it will never
; be further away than the `current` node.
(struct (A) bus ([current : (fnode A)]
                 [latest : (fnode A)])
  #:property prop:authentic #t
  #:type-name Busof)

(define node-next fnode-next)

(define-syntax-rule (make-false-box A)
  (ann (box #f) (Boxof (U #f (node A)))))

(: make-node (All (A) (-> A (node A))))
(define (make-node item)
  (node (make-false-box A) item))

(: node-last (All (A) (-> (fnode A) (fnode A))))
; Returns the last node in the list
(define (node-last n)
  (let* ([next (unbox (node-next n))])
    (if next
        (node-last next)
        n)))

(: node-enqueue! (All (A) (-> (fnode A) A (node A))))
; Adds the item to the end of the linked list.
; Returns the new final node containing that item.
(define (node-enqueue! n item)
  (let* ([final (make-node item)]
         [last (node-last n)]
         [result (box-cas! (node-next last) #f final)])
    (if result
        final
        (node-enqueue! last item))))

(: make-bus (All (A) (-> (Busof A))))
(define (make-bus)
  (let ([empty (fnode (make-false-box A))])
    (bus empty empty)))

(: bus-dequeue (All (A) (-> (Busof A) (U #f (Pairof (Busof A) A)))))
(define (bus-dequeue the-bus)
  (let* ([current (bus-current the-bus)]
         [latest (bus-latest the-bus)]
         [next (unbox (node-next current))])
    (if next
        (cons (bus next (node-last latest))
              (node-item next))
        #f)))

(: bus-enqueue! (All (A) (-> (Busof A) A (Busof A))))
(define (bus-enqueue! the-bus item)
  (let* ([latest (bus-latest the-bus)]
         [latest (node-enqueue! latest item)])
    (struct-copy bus the-bus [latest latest])))

(: bus-discard-all (All (A) (-> (Busof A) (Busof A))))
; Skips to the end of the list.
(define (bus-discard-all the-bus)
  (let* ([latest (bus-latest the-bus)]
         [latest (node-last latest)])
    (bus latest latest)))

(: bus->list (All (A) (-> (Busof A) (values (Listof A) (Busof A)))))
(define (bus->list the-bus)
  (: go (-> (Busof A) (Listof A) (values (Listof A) (Busof A))))
  (define (go the-bus accum)
    (let ([result (bus-dequeue the-bus)])
      (if result
          (go (car result)
              (cons (cdr result) accum))
          (values (reverse accum) the-bus))))
  (go the-bus '()))

(module+ test
  (require typed/rackunit)

  (: bus-dq (All (A) (-> (Busof A) (values A (Busof A)))))
  ; Version of bus-dequeue that requires success
  (define (bus-dq b)
    (let ([result (bus-dequeue b)])
      (assert result)
      (values (cdr result) (car result))))

  (let* ([orig-bus (make-bus)]
         [_ (bus-enqueue! orig-bus 300)]
         [(item b2) (bus-dq orig-bus)]
         [_ (begin
              (check-equal? item 300)
              (bus-enqueue! orig-bus 400)
              (bus-enqueue! b2 500))]
         [(item b2) (bus-dq b2)]
         [_ (check-equal? item 400)]
         [(item b2) (bus-dq b2)]
         [_ (check-equal? item 500)]
         [_ (check-equal? (bus-dequeue b2) #f)]
         [(lst _) (bus->list orig-bus)]
         [_ (check-equal? lst '(300 400 500))]
         )
    (void)))
