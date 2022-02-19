#lang typed/racket

(provide make-loc
         ; Occupants
         occupant? occupant-color
         catalyst? catalyst-direction
         fuel? ground? contaminant?
         ; State
         State state? make-initial-state
         state-get state-apply state-penalty-state
         state-settings state-width state-height state-next-spawns state-game-over?
         state-energy state-max-energy state-stats state-current-combo state-previous-combo
         ; PenaltyState
         PenaltyState penalty-state? penalty-state-countdown
         ; Actions
         Action
         (prefix-out action: (combine-out move jump rotate tick plummet burst drop-keydown drop-keyup))
         ; Combo
         Combo combo? combo-total-payout combo-explanations
         ; Stamped
         Stamped stamp? make-stamp
         stamped? stamped-value stamped-pid stamped-client-timestamp
         ; GameSettings
         GameSettings game-settings? default-game-settings game-settings-penalty:resistance
         settings->assoc assoc->settings
         ; Frame
         Frame frame? frame-state frame-counter frame-info frame-timing frame-waiting?
         make-first-frame next-frame frame-do-action frame-extra-occs frame-time-remaining
         ; Timing
         Timing timing? timing-bursting
         ; Stats
         Stats stats?
         stats-spawn-count stats-spawn-energy stats-waiting-frames stats-waiting-energy
         ; GameLog
         GameLog gamelog? read-gamelog gamelog->replay
         ; AI
         choose-move

         ; === below this line is only used in multiplayer ===
         write-dto read-dto
         Pid SnapId

         ; requests and responses:
         Request rq? Response rs?
         (filtered-out (build-filter "rq:" "rs:")
                       (all-from-out "core/data.rkt"))
         ; Token
         Token token? token-pid
         ; Server
         make-server server-start server-start-2 server-stop server-begin-game
         server-set-mp-settings server-player-infos
         ; Replay
         Replay replay? make-replay
         replay-frame replay-enqueue! replay-advance!
         ; PlayerSettings
         PlayerSettings player-settings? make-player-settings
         player-settings-name
         ; PlayerInfo
         PlayerInfo player-info?
         player-info-start-game-payload player-info-pid player-info-settings
         player-info-name player-info-ready?
         ; MultiplayerSettings
         MultiplayerSettings multiplayer-settings?
         make-multiplayer-settings default-multiplayer-settings
         multiplayer-settings-game-settings multiplayer-settings-viewmodel
         multiplayer-settings-competition-type merge-settings
         )

(require racket/provide
         "core/ai.rkt"
         "core/data.rkt"
         "core/frame.rkt"
         "core/gamelog.rkt"
         "core/replay.rkt"
         "core/state.rkt"
         "core/server.rkt"
         )

(begin-for-syntax
  (define (a . starts-with . b)
    (string=? (substring a 0 (min (string-length a)
                                  (string-length b)))
              b))
  (define-syntax-rule (build-filter prefix ...)
    (lambda (name)
      (and (or (name . starts-with . prefix)
               ...)
           ;(println name)
           name))))
