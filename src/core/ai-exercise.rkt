#lang typed/racket

; This file makes the AI many games to completion, and verifies that each
; game is completed and that the average score does not regress.
; This can take a long time to run, so we use places.

(module worker-impl typed/racket
  ; This module implements run-trial which accepts a game-settings-assoc,
  ; runs the game to completion, and returns the final score.
  (provide run-trial)

  (require "../core.rkt"
           "../typed-utils.rkt"
           (only-in "ai.rkt" choose-move))

  (: fast-forward (-> State State))
  (define (fast-forward state)
    (let* ([result (state-apply state (list (action:tick)))]
           [new-state (car result)])
      (if new-state
          (fast-forward new-state)
          state)))

  (: play (-> State Integer))
  (define (play state)
    (define-syntax-rule (on-error x ...)
      (begin
        (println (settings->assoc (state-settings state)))
        (error x ... )))
    (let* ([state (fast-forward state)]
           [game-over? (state-game-over? state)]
           #:break (when game-over?
                     (case game-over?
                       [(win) (state-energy state)]
                       [else (on-error "game-lost:" game-over?)]))
           [choice (choose-move state)]
           #:break (when (not choice)
                     (on-error "failed to choose move"))
           [seq (second choice)]
           [result (state-apply state seq)]
           [new-state (car result)]
           #:break (when (not new-state)
                     (on-error "failed to apply move seq")))
      (play new-state)))

  (: run-trial (-> Any Integer))
  (define (run-trial settings-assoc)
    (let* ([settings (assoc->settings settings-assoc)]
           [state (make-initial-state settings)])
      (play state))))

(module worker-channel racket
  ; I'm not sure why this code needs to be untyped. Maybe a TR bug?
  (provide make-worker)

  (require (submod ".." worker-impl))

  #;(: make-worker (-> Place))
  ; Creates a place expecting a request message of the form
  #;(list game-settings-assoc anything ...)
  ; The response message will be
  #;(cons game-final-score echoed-request-message)
  ; Also, a request message of #f terminates the place
  (define (make-worker)
    (place ch
           (define (loop)
             (define arg (place-channel-get ch))
             (when arg
               (let* ([score (run-trial (first arg))])
                 (place-channel-put ch (cons score arg))
                 (loop))))
           (loop))))

; Can be run using `raco test -s slow-test [more ...]`
(module+ slow-test
  (require typed/racket/async-channel
           typed/rackunit)
  (require/typed (submod ".." worker-channel)
                 [make-worker (-> Place)])

  ; This channel acts as a queue for workers that become available.
  (define worker-queue (ann (make-async-channel)
                            (Async-Channelof Place-Channel)))

  ; Create workers and add them to the queue.
  ; Also hold onto them in this list so we can clean them up later.
  (define all-workers : (Listof Place)
    (for/list ([i (in-range 8)])
      (let ([worker (make-worker)])
        (async-channel-put worker-queue worker)
        worker)))

  (: start-work (-> Any (Evtof Any)))
  ; Blocks until there is an available worker.
  ; Then sends that worker the given request message and returns an evt whose
  ; sync result is the response message from the worker.
  (define (start-work request-msg)
    (let ([worker (async-channel-get worker-queue)] ; block until we get one
          [response-msg : Any #'unreachable-value])
      (wrap-evt (thread
                 (lambda ()
                   (place-channel-put worker request-msg)
                   (set! response-msg (place-channel-get worker))
                   ; add this worker back into the queue
                   (async-channel-put worker-queue worker)))
                (lambda args response-msg))))

  (: run-trials (-> Integer (-> Any Any) Integer Void))
  ; Runs the given number of trials and assert that the average score meets or
  ; exceeds expectations.
  (define (run-trials count settings-func min-avg-score)
    (define layout-name (object-name settings-func))
    (define all-evts : (Listof (Evtof Any))
      ; Use a hard-coded seed to avoid bad luck breaking the test.
      ; (Some seeds will be legitimately more difficult than others.)
      (let ([prng (vector->pseudo-random-generator
                   '#(1970691962 3632932342 2858274971 363470546
                                 1753236688 2592668703))])
        (for/list ([i (in-range count)])
          (when (= 0 (remainder i 10))
            (println (format "started ~a of ~a runs, ~a"
                             i count layout-name)))
          (let* ([_ (random 1 prng)] ; advance PRNG
                 [rand-vec (pseudo-random-generator->vector prng)]
                 [settings-assoc (settings-func rand-vec)])
            (start-work (list settings-assoc))))))
    (define total-score
      (for/fold ([sum : Integer 0])
                ([evt (in-list all-evts)])
        (let ([result (sync evt)])
          (match result
            [(list score echoed-request ...)
             #:when (exact-integer? score)
             (+ sum score)]
            [else
             (error "bad result:" result)]))))
    (println (format "completed ~a runs, ~a" count layout-name))
    (define avg-score (truncate (/ total-score count)))
    (let ([msg (format "~a average score is ~a (expected >= ~a)"
                       layout-name avg-score min-avg-score)])
      (if (avg-score . < . min-avg-score)
          (fail msg)
          (println msg)))
    (void))

  (define (mini-layout-settings seed)
    `((layout:mode . mini)
      (layout:fuel-height . 5)
      (layout:fuel-count . 12)
      (layout:num-waves . 1)
      (time-attack:type . #f)
      (time-attack:seconds . 600)
      (energy:initial . 10000)
      (energy:max . 10000)
      (energy:catalyst-cost . 55)
      (energy:drain-rate . 0)
      (energy:combo-payout . #(0 200 500 1000 2000))
      (energy:fuel-value . 100)
      (energy:horizontal-bonus-factor . 20)
      (energy:wave-completion-bonus . 5000)
      (penalty:type . #f)
      (penalty:resistance . 1000)
      (penalty:catalyst-cost . 0)
      (penalty:floor-drop-amount . 4)
      (attack:combo-payout . #(0 50 100 200 500 1000))
      (attack:horizontal-bonus-factor . 50)
      (misc:catalyst-deck . blank-12)
      (misc:combos-require-fuel? . #f)
      (misc:random-seed . ,seed)))

  (define (standard-layout-settings seed)
    `((layout:mode . standard)
      (layout:fuel-height . 13)
      (layout:fuel-count . 84)
      (layout:num-waves . 1)
      (time-attack:type . #f)
      (time-attack:seconds . 600)
      (energy:initial . 40000)
      (energy:max . 40000)
      (energy:catalyst-cost . 250)
      (energy:drain-rate . 0)
      (energy:combo-payout . #(0 200 500 1000 2000))
      (energy:fuel-value . 100)
      (energy:horizontal-bonus-factor . 20)
      (energy:wave-completion-bonus . 5000)
      (penalty:type . #f)
      (penalty:resistance . 1000)
      (penalty:catalyst-cost . 0)
      (penalty:floor-drop-amount . 4)
      (attack:combo-payout . #(0 50 100 200 500 1000))
      (attack:horizontal-bonus-factor . 50)
      (misc:catalyst-deck . blank-12)
      (misc:combos-require-fuel? . #f)
      (misc:random-seed . ,seed)))

  (run-trials 1000 mini-layout-settings 10690)
  (run-trials 1000 standard-layout-settings 22000)

  ; Clean up all the places
  (for ([worker all-workers])
    (place-channel-put worker #f)
    (when (not (sync/timeout 1.0 (place-dead-evt worker)))
      (println "place did not exit cleanly, killing...")
      (place-kill worker)))

  (println (format "~a, tests completed"
                   (syntax-source #'here)))
  )
