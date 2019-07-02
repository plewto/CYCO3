;;;; CYCO
;;;;

(global *os-path-root* "/")
(global *os-path-separator* #\/)
(global *os-extension-separator* #\.)
(global *os-homedir-alias* #\~)

;; Default project
;;
(global *ticks-per-beat* 480) 
(global *default-project-directory* "~/cyco-projects")
(global *project-main-filename-format* "~A-main")
(global *default-project-output-directory* "MIDI")
(global *project* nil)
(global *metronome* nil)
(global *countin* nil)
(global *endpad* nil)  ;; ISSUE: should this be depreciated?


;; Plugins
;;
(global *config-directory* nil)
(global *plugin-directory* nil)
(global *current-plugin* nil)
