#lang racket/gui

(provide host-frame%)

(require "../core.rkt"
         "../util/assert.rkt"
         "setup-panel.rkt"
         "occupant-picts.rkt"
         )

; Returns a method-dispatching procedure.
; Constructs a panel that allows the user to enter the port number and start the server.
(define (make-connect-panel
         #:parent parent
         ; on-start-server is a callback of (-> listen-port-number? any).
         ; It is called when the Start Server button is clicked.
         #:on-start-server on-start-server)
  (define panel (new vertical-panel% [parent parent]))
  (define port-field (new text-field%
                          [parent panel]
                          [min-width 100]
                          [stretchable-width #f]
                          [label "Port"]
                          [init-value "5577"]))
  (define (get-port-number)
    (let* ([str (send port-field get-value)]
           [port (string->number str)])
      (and (listen-port-number? port) port)))
  (define start-button
    (new button%
         [parent panel]
         [label "Start Server"]
         [callback (lambda args
                     (let ([port (get-port-number)])
                       (if port
                           (on-start-server port)
                           (set-error "Invalid port number"))))]))
  (define error-message (new message%
                             [parent panel]
                             [label ""]
                             ; Need auto-resize, otherwise it stays at zero forever
                             [auto-resize #t]))
  (define (set-error msg)
    (send error-message set-label msg))
  (lambda args
    (match args
      [(list 'set-error-message msg)
       (set-error msg)]
      [else
       (fail "unknown method" args)])))

; Returns a method-dispatching procedure.
; Constructs a panel that allows the user to configure the game settings.
(define (make-config-panel
         #:parent parent
         #:on-settings-changed on-settings-changed)
  (define panel (new vertical-panel%
                     [parent parent]))
  (define (get-mp-settings setup-panel)
    (send setup-panel make-mp-settings))
  (define setup-panel
    (new setup-panel%
         [parent panel]
         [host-mode? #t]
         [setting-changed-callback
          (lambda (self . more)
            (on-settings-changed (get-mp-settings self)))]))
  (lambda args
    (match args
      [(list 'get-mp-settings)
       (get-mp-settings setup-panel)]
      [else
       (fail "unknown method" args)])))

(define (infinite-stream val)
  (stream-cons val (infinite-stream val)))

(define false-stream (infinite-stream #f))

; Creates a player to be shown in the list of connected players.
; Returns an update-proc which accepts a player-info and refreshes the UI.
(define (make-player parent pi)
  (define cb (new check-box%
                  [label ""]
                  [stretchable-width #f]
                  [min-width 200]
                  [parent parent]))
  (define (update pi)
    (let ([name (player-info-name pi)]
          [ready? (player-info-ready? pi)])
      (send cb set-label name)
      (send cb set-value ready?)))
  (update pi)
  update)

; Returns a method-dispatching procedure.
; Constructs a panel that shows all the players who have connected to the server.
(define (make-player-info-panel
         #:parent parent
         #:on-start-game on-start-game)
  (define panel (new vertical-panel%
                     [alignment '(center top)]
                     [parent parent]))
  (define player-panel (new vertical-panel%
                            [alignment '(left top)]
                            [stretchable-height #f]
                            [parent panel]))
  (define players (list)) ; list of `(make-player ...)` return values
  (define start-game-button
    (new button%
         [parent panel]
         [label "Start Game"]
         [callback on-start-game]))
  (lambda args
    (match args
      [(list 'update-players server)
       (let* ([pis (server-player-infos server)]
              [done? #f]
              [new-players
               (for/list ([pi pis]
                          [player (in-stream (stream-append players false-stream))])
                 (cond
                   [(and pi player)
                    (begin (player pi)
                           player)]
                   [pi
                    (make-player player-panel pi)]
                   [else (fail "incomplete cond" pi player)]))])
         (set! players new-players)
         (send player-panel change-children
               (lambda (kids) (take kids (length new-players)))))]
      [else
       (fail "unknown method" args)])))

(define host-frame%
  (class frame%
    (super-new [label "Game Server"])
    ; The custodian will be shut down when the frame is closed.
    ; This will terminate the server and any stray listener threads.
    (init-field [custodian (make-custodian)])
    ; listener : (or/c #f tcp-listener?)
    (define listener #f)
    ; listen-thread : (or/c #f thread?)
    ; The thread that we call `start-server` on; sits in a `tcp-accept` loop
    ; until we shut it down.
    (define listen-thread #f)
    ; server : (or/c #f Server)
    (define server #f)
    ; refresh-timer : (or/c #f timer%)
    ; Used to update the UI to show updated client data
    (define refresh-timer #f)

    (define-syntax-rule (def (head ...) body ...)
      (define (head ...)
        (parameterize ([current-custodian custodian])
          body ...)))

    (def (update-server-settings mps)
      (let ([server server])
        (when server
          ;(println (list "server set:" mps))
          (server-set-mp-settings server mps))))

    (define (refresh-timer-tick . args)
      (let ([server server])
        (when server
          (player-info-manager 'update-players server))))

    (def (set-listener l)
      ; This is a little more complicated than it needs to be because we used
      ; to allow re-starting a new listener using the same host-frame%.
      ; We don't allow this any more, but the code still works.
      (when refresh-timer
        (send refresh-timer stop)
        (set! refresh-timer #f))
      (when server
        (server-stop server)
        (set! server #f))
      (when listen-thread
        (let ([result (sync/timeout 2 listen-thread)])
          (when (not result)
            (println "failed to shut down cleanly, killing...")
            (kill-thread listen-thread)))
        (set! listen-thread #f))
      (when listener
        (with-handlers ([any/c (lambda args (void "ignore"))])
          (tcp-close listener))
        (set! listener #f))
      (set! listener l)
      (define listening? (and listener #t))
      (when listener
        (set! server (make-server))
        (update-server-settings (config-manager 'get-mp-settings))
        (let ([server server])
          (set! listen-thread (thread (lambda ()
                                        (server-start-2 server listener))))
          (set! refresh-timer (new timer%
                                   [interval 200]
                                   [notify-callback refresh-timer-tick])))))

    (def (start-server port)
      (assert (not listener))
      (set-listener (tcp-listen port))
      (send this change-children (lambda args (list panel-b)))
      (send this create-status-line)
      (send this set-status-text
            (format "Listening on port ~a. Close window to shut down." port)))

    (def (start-game button event)
      (assert server)
      (server-begin-game server)
      (send panel-b enable #f)
      (send this set-status-text "Game in progress. Close window to shut down."))

    ; Panel A will be used to start a server, at which time we will transition
    ; to Panel B which will allow game configuration and viewing other players.
    (define panel-a (new panel%
                         [parent this]))
    (define panel-b (new horizontal-panel%
                         [style '(deleted)]
                         [spacing 50]
                         [parent this]))

    (define connect-manager
      (make-connect-panel #:parent panel-a
                          #:on-start-server start-server))

    (define config-manager
      (make-config-panel #:parent panel-b
                         #:on-settings-changed update-server-settings))

    (define player-info-manager
      (make-player-info-panel #:parent panel-b
                              #:on-start-game start-game))

    (define/override (on-superwindow-show show?)
      (if show?
          ; If the custodian is shut down, this frame cannot be reused.
          ; The caller should create a new frame instead.
          (assert (not (custodian-shut-down? custodian)))
          (begin
            (set-listener #f)
            (custodian-shutdown-all custodian))))

    (send this set-icon host-icon)
    ))

(module+ main
  (define frame (new host-frame%))
  (send frame show #t))
