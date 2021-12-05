#lang typed/racket

(provide GameLog gamelog? read-gamelog gamelog->replay)

(require "../typed-utils.rkt"
         "data.rkt"
         "data-helpers.rkt"
         "replay.rkt")

; A GameLog is guaranteed to be compatible with the current version of the app,
; even if the saved data is from an older version of the app.
(struct gamelog ([first-frame : Frame]
                 [actions : (Listof (Stamped Action))]
                 [last-frame : (U #f Frame)])
  #:type-name GameLog
  #:transparent)

(: gamelog->replay (-> GameLog Replay))
(define (gamelog->replay gamelog)
  (let* ([first-frame (gamelog-first-frame gamelog)]
         [replay (make-replay first-frame)]
         [actions (gamelog-actions gamelog)])
    (replay-enqueue! replay actions)
    replay))

; =====
; Within this file, the special serialization we do in `write-dto` is relevant.
; For example, it uses "C" to indicate cons and "L" to indicate list.
; ====

; Reads a gamelog, typically from the filesystem.
(: read-gamelog (-> Input-Port GameLog))
(define (read-gamelog port)
  (define frame-count? (make-predicate FrameCount))

  (: read-datums (-> Input-Port (Listof Any)))
  (define (read-datums port)
    (let ([result (read-datum port)])
      (if (eof-object? result)
          (list)
          (cons result (read-datums port)))))

  (: parse-actions (-> (Listof Any) (Listof (Stamped Action))
                       (Values (Listof (Stamped Action)) (Listof Any))))
  (define (parse-actions datums accum)
    (match datums
      [(list `(C ,frame-num ,action-datum)
             more ...)
       #:when (frame-count? frame-num)
       (let* ([action (datum->dto action-datum)]
              #:break (when (not (action? action))
                        (fail "expected an action, but got:" action-datum))
              [stamp (make-stamp #f #f frame-num)]
              [sa (cons stamp action)])
         (parse-actions more (cons sa accum)))]
      [else
       (values (reverse accum) datums)]))

  (: parse-v1 (-> (Listof Any) GameLog))
  (define (parse-v1 datums)
    (match datums
      [(list `(L '#:first-frame ,frame-datum)
             more ...)
       (let* ([frame (datum->dto frame-datum)]
              #:break (when (not (frame? frame))
                        (fail "expected a frame, but got" frame-datum))
              [(actions more)
               (parse-actions more '())]
              [last-frame
               (match more
                 [(list `(L '#:last-frame ,frame-datum))
                  (let* ([frame (datum->dto frame-datum)])
                    (or (and (frame? frame) frame)
                        (fail "expected a frame, but got" frame-datum)))]
                 [(list)
                  #f]
                 [else
                  (fail "unexpected data after actions")])])
         (gamelog frame actions last-frame))]
      [else
       (fail "invalid gamelog v1 (missing first frame)")]))

  (: parse-log (-> (Listof Any) GameLog))
  (define (parse-log datums)
    (match datums
      [(list `(L '#:version ,version)
             more ...)
       (case version
         [(1)
          (parse-v1 more)]
         [else
          (fail "invalid gamelog version:" version)])]
      [else
       (fail "invalid replay data (missing version)")]))

  (parse-log (read-datums port)))


(module+ test
  (require (except-in typed/rackunit fail))

  ; Test that these replays reach the expected final frame.
  (define validate-dir "../../recordings/to-validate")
  (for ([filename '(1 2 3)])
    (let* ([path (format "~a/~a.ffreplay" validate-dir filename)]
           [port (open-input-file path)]
           [gamelog (read-gamelog port)]
           [_ (close-input-port port)]
           [replay (gamelog->replay gamelog)]
           [log-last-frame (gamelog-last-frame gamelog)]
           #:break (when (not log-last-frame)
                     (fail "missing last frame" path))
           [last-frame-num (frame-counter log-last-frame)])
      (replay-fast-forward! replay last-frame-num)
      (check-true (equal? log-last-frame (replay-frame replay)))))
  )
