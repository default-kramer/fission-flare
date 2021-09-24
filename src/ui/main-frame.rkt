#lang racket/gui

; Future Work:
; * improve anchoring/catchup/replay-lag
; * create docs
;   - how to install/run (virus warning)
;   - how to play
;   - mention multiplayer > 2
; * make sure controller feels OK
; * wave completion should be more obvious (the last wave can be hard to be sure)
;   - probably should clear the half-grid even when there is no incoming wave
; * Puzzle Trainer mode
; * play vs CPU
; * audio maybe?


(require pict
         "../util/assert.rkt"
         "../core.rkt"
         "client-connection.rkt"
         "controller.rkt"
         "host-frame.rkt"
         "setup-panel.rkt"
         "occupant-picts.rkt"
         )

(define host-frame #f)

; An evt? whose synchronization result is a response from the server.
; We only have one request in-flight at a time.
(define rq-evt #f)

(define active-child #f)
(define (set-child x)
  (send main-container change-children (lambda args (list)))
  (send main-container add-child x)
  (set! active-child x))

(define (in-lobby?) (eq? active-child lobby-panel))
(define (in-game?) (eq? active-child game-canvas))

(define (run-game-loop)
  ; Warning - this frame-rate must match what is in frame-timing.
  ; I should probably make this a constant.
  ; There's no need for a higher frame rate with the current graphics.
  (define frame-rate 30)
  (define millis-per-frame (/ 1000.0 frame-rate))
  (define frame-count #f)
  (define start-time #f)
  (define (re-anchor!)
    (set! frame-count 0)
    (set! start-time (current-inexact-milliseconds))
    (println (list "re-anchor:" start-time)))
  (define (get-sleep-time!)
    (set! frame-count (add1 frame-count))
    (define due-time (+ start-time (* millis-per-frame frame-count)))
    (define sleep-time (- due-time (current-inexact-milliseconds)))
    (/ sleep-time 1000.0))
  (define (catchup)
    ((controller 'next-frame!))
    (let ([time (get-sleep-time!)])
      (if (> time 0)
          (begin (repaint)
                 (sleep/yield time))
          (catchup))))

  (define (loop)
    (let ([controller controller])
      (when controller
        ((controller 'next-frame!))
        (repaint)
        (define sleep-time (get-sleep-time!))
        (cond
          [(> sleep-time 0)
           (sleep/yield sleep-time)]
          [(> sleep-time -0.33)
           ; If we're only a few frames behind target, fast forward
           (catchup)]
          [else
           (begin (re-anchor!)
                  (sleep/yield (max 0 (get-sleep-time!))))])
        (loop))))
  (re-anchor!)
  (loop))

(define controller #f)
(define (start-game ctrl)
  (set! controller ctrl)
  (set-child game-canvas)
  (run-game-loop))

(define conn #f)

(define (timer-tick)
  (let ([conn conn])
    (when conn
      (use-connection conn))))

(define timer (new timer% [notify-callback timer-tick]))
(send timer start 150)

(define (cleanup)
  (set! controller #f)
  (let ([conn conn])
    (when conn
      (client-disconnect conn)))
  (set! conn #f))

(define (use-connection conn)
  (cond
    ; If there is an in-flight request, see if the response has arrived
    [rq-evt
     (let ([rs (with-handlers ([any/c (lambda args
                                        ; How to update button text back to "Connect" ?
                                        (set! rq-evt #f)
                                        #f)])
                 (sync/timeout 0 rq-evt))])
       (when rs
         (set! rq-evt #f)
         (process-rs rs conn)))]
    ; If we are in the lobby, do a poll
    [(in-lobby?)
     (let* ([player-settings (lobby-cmd 'get-player-settings)]
            [rq (rq:poll-lobby player-settings)])
       (set! rq-evt (sendrq rq conn)))]
    ))

(define (sendrq rq [conn conn])
  (assert conn)
  (client-sendrq conn rq))

(define (process-rs rs conn)
  (match rs
    [(rs:PollLobby infos mp-settings)
     ; Careful - using `ormap` might mean that some players have started the
     ; game but others have not! So use `andmap` instead.
     ; (The server should probably be improved.)
     (let ([game-started? (andmap player-info-start-game-payload infos)])
       (lobby-cmd 'poll-complete mp-settings)
       (when game-started?
         (let* ([my-pid (client-pid conn)]
                [controller (relaxed-multi-player-game infos my-pid sendrq)])
           (start-game controller))))]
    [else
     (fail "unhandled rs:" rs)]))

(define (repaint)
  (send game-canvas refresh))

(define (paint canvas dc)
  (when controller
    (let*-values ([(cw ch) (send canvas get-client-size)]
                  [(pict)
                   ((controller 'get-pict) cw ch)])
      (draw-pict pict dc 0 0))))


; If you hold the drop button too long, the keyboard repeat kicks in and you
; might accidentally immediately drop your next catalyst.
; So use this flag to detect and solve that annoyance.
(define holding-drop-button? #f)

(define (key-event->action key-event)
  (let* ([code (send key-event get-key-code)]
         [shift? (send key-event get-shift-down)])
    (case code
      [(left)
       (if shift?
           (action:jump 'l)
           (action:move 'l))]
      [(right)
       (if shift?
           (action:jump 'r)
           (action:move 'r))]
      [(#\d #\D)
       (action:rotate #f)]
      [(#\f #\F)
       (action:rotate #t)]
      [(rshift)
       (if holding-drop-button?
           #f
           (begin (set! holding-drop-button? #t)
                  (action:drop-keydown)))]
      [(release)
       ; The key release code (shift) does not match the key down code (rshift)
       ; I think this is a Racket bug https://github.com/racket/gui/issues/236
       ; This might cause a problem, but so far I haven't noticed it.
       (if (equal? 'shift (send key-event get-key-release-code))
           (begin (set! holding-drop-button? #f)
                  (action:drop-keyup))
           #f)]
      [else #f])))

(define game-frame%
  (class frame%
    (super-new)
    (define/override (on-subwindow-char receiver key-event)
      (and (in-game?)
           (let ([action (key-event->action key-event)])
             (when (and action controller)
               ((controller 'do-action!) action)
               (repaint))
             #t)))
    (define-syntax-rule (ignore-errors body ...)
      (with-handlers ([any/c (lambda args (void))])
        body ...))
    (define/override (on-superwindow-show show?)
      (when (not show?)
        (ignore-errors (cleanup))
        (ignore-errors
         (when host-frame
           (send host-frame show #f)
           (set! host-frame #f)))
        ; This closes the console window:
        (exit)))
    ))

(define frame
  (new game-frame%
       [label "Fission Flare"]
       [min-width 1100]
       [min-height 800]))

(define wrapper-panel
  (new vertical-panel%
       [parent frame]
       [border 10]
       [spacing 25]))

(define back-to-title (lambda args
                        (cleanup)
                        (set-child main-panel)))

(define-syntax-rule (vn x ...)
  (void (new x ...)))

(vn button%
    [parent wrapper-panel]
    [label "Back to Title"]
    [callback back-to-title])

(define main-container
  (new panel%
       [parent wrapper-panel]))

(define main-panel
  (new vertical-panel%
       [parent main-container]))

(define game-canvas
  (new canvas%
       [parent main-container]
       [paint-callback paint]))

(vn button%
    [parent main-panel]
    [label "Single-Player"]
    [callback (lambda args
                (set-child single-player-setup-panel))])

(vn button%
    [parent main-panel]
    [label "Multi-Player"]
    [callback (lambda args
                (set-child lobby-panel))])

(vn button%
    [parent main-panel]
    [label "Host Multi-Player"]
    [callback
     (lambda args
       (if (and host-frame
                (send host-frame is-shown?))
           (void) ; already open
           (begin
             (set! host-frame (new host-frame%
                                   [min-width 300]
                                   [min-height 300]))
             (send host-frame show #t))))])

(let ([wrapper (new vertical-panel%
                    [parent main-panel]
                    [alignment '(left bottom)])]
      [e1 "fission"]
      [e2 "flare"])
  (define (msg str)
    (vn message%
        [parent wrapper]
        [vert-margin 0]
        [label str]))
  (msg "Copyright 2021 Ryan Kramer")
  (msg "")
  (msg "If you intend to commercially release a clone or near-clone of this game,")
  (msg (format "please let me know at ~a.~a@gmail.com" e1 e2))
  (msg "so that I don't duplicate your work."))

{begin ; single-player
  (define single-player-setup-panel
    (let ()
      (define (start-the-game . args)
        (let* ([settings (send sp make-game-settings)]
               [state (make-initial-state settings)])
          (start-game (relaxed-single-player-game state))))
      (define sp (new setup-panel%
                      [parent main-container]
                      [start-game-callback start-the-game]))
      sp))
  }

(define (get-username)
  (define result
    (environment-variables-ref (current-environment-variables) #"username"))
  (and result (bytes->string/utf-8 result)))

(define-values (lobby-panel lobby-cmd)
  (letrec ([this (new vertical-panel%
                      [parent main-container]
                      [stretchable-width #f]
                      [alignment '(center top)])]
           [connect-panel
            (new horizontal-panel%
                 [parent this]
                 [border 25]
                 [stretchable-height #f])]
           [host-text
            (new text-field%
                 [parent connect-panel]
                 [label "Host"]
                 [init-value "localhost"])]
           [port-text
            (new text-field%
                 [parent connect-panel]
                 [label "Port"]
                 [init-value "5577"])]
           [toggle-connect
            (lambda args
              (cond
                [conn
                 (begin
                   (cleanup)
                   (send connect-button set-label "Connect"))]
                [else
                 (let ([host (send host-text get-value)]
                       [port (string->number (send port-text get-value))])
                   (begin-busy-cursor)
                   (thread
                    (lambda ()
                      (with-handlers ([any/c (lambda (e)
                                               (end-busy-cursor)
                                               (raise e))])
                        (set! conn (client-connect! host port))
                        (end-busy-cursor)
                        (send connect-button set-label "Disconnect")))))]))]
           [connect-button
            (new button%
                 [parent connect-panel]
                 [label "Connect"]
                 [callback (lambda args (toggle-connect))])]
           [username-field
            (new text-field%
                 [parent this]
                 [label "Player Name"]
                 [init-value (or (get-username)
                                 (format "~a" (gensym 'player)))])]
           [ready-cb
            (new check-box%
                 [label "Ready"]
                 [parent this])]
           [_ (new panel% ; just a spacer
                   [parent this]
                   [vert-margin 20])]
           [setup-panel
            (new setup-panel%
                 [parent this]
                 [constraints default-multiplayer-settings])])
    (define (lobby-cmd . args)
      (match args
        [(list 'get-player-settings)
         (let* ([game-settings (send setup-panel make-game-settings)]
                [name (send username-field get-value)]
                [ready? (send ready-cb get-value)])
           (make-player-settings name ready? game-settings))]
        [(list 'poll-complete mp-settings)
         ; set-constraints returns #t when something changed
         (when (send setup-panel set-constraints mp-settings)
           (send ready-cb set-value #f))]
        [else (fail "bad args" args)]))
    (values this lobby-cmd)))

(module+ main
  (set-child main-panel)
  (void (send frame set-icon main-icon))
  (send frame show #t))
