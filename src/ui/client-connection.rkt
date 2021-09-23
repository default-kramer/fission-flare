#lang typed/racket

(provide ClientConnection client-connection? client-connect! client-disconnect
         client-sendrq client-pid)

(require "../typed-utils.rkt"
         "../core.rkt")

(struct connection ([in : Input-Port]
                    [out : Output-Port]
                    ; locker is used to ensure that only 1 request is in flight at a time.
                    [locker : Semaphore]
                    ; limiter is used to ensure that a max of N requests are pending.
                    ; (The in-flight request counts as pending, plus any requests
                    ;  that are waiting behind it.)
                    [limiter : Semaphore])
  #:type-name Connection
  #:transparent)

(struct client-connection connection ([token : Token])
  #:type-name ClientConnection
  #:transparent)

(: client-connect! (->* (String Integer) (#:token (U #f Token)) ClientConnection))
; Blocks the current thread for up to 5 seconds.
(define (client-connect! host port #:token [token #f])
  (: do-it (-> ClientConnection))
  (define (do-it)
    (let* ([(in out)
            (tcp-connect host port)]
           [locker (make-semaphore 1)]
           [limiter (make-semaphore 1)]
           [rq : Request (if token
                             (rq:become-pid token)
                             (rq:get-pid))]
           [evt (or (client-sendrq (connection in out locker limiter) rq)
                    (fail "couldn't send first request"))]
           [rs (sync/timeout 5 evt)]
           #:break (when (not rs)
                     (fail "no response to first request"))
           [token (extract-token rs)])
      ;(println (list "client connection established" host port token))
      (client-connection in out locker limiter token)))
  (let ([cust (make-custodian)])
    (parameterize ([current-custodian cust])
      (with-handlers ([exn? (lambda ([e : exn])
                              (custodian-shutdown-all cust)
                              (raise e))])
        (do-it)))))

(: client-disconnect (-> Connection Void))
(define (client-disconnect conn)
  (close-output-port (connection-out conn)))

(: extract-token (-> Response Token))
(define (extract-token rs)
  (if (rs:got-pid? rs)
      (rs:got-pid-token rs)
      (fail "unexpected response while establishing PID" rs)))

(: client-sendrq (-> Connection Request (U #f (Evtof Response))))
(define (client-sendrq conn rq)
  (let ([limiter (connection-limiter conn)]
        [locker (connection-locker conn)]
        [result : (Boxof (U #f Response)) (box #f)])
    (and (semaphore-try-wait? limiter)
         (wrap-evt (thread (lambda ()
                             (semaphore-wait locker)
                             (let ([rs (send/recv conn rq)])
                               (semaphore-post locker)
                               (semaphore-post limiter)
                               (set-box! result rs))))
                   (lambda args
                     (or (unbox result)
                         (fail "failed to send/recv:" rq)))))))

(: send/recv (-> Connection Request Response))
(define (send/recv conn rq)
  (let* ([out (connection-out conn)]
         [in (connection-in conn)]
         [_ (write-dto rq out)]
         [rs (read-dto in)])
    (if (rs? rs)
        rs
        (fail "unexpected response from server" rs))))

(: client-pid (-> ClientConnection Pid))
(define (client-pid conn)
  (token-pid (client-connection-token conn)))
