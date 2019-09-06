;;;; CYCO
;;;;

(global *os-path-root* "/")
(global *os-path-separator* #\/)
(global *os-extension-separator* #\.)
(global *os-homedir-alias* #\~)

;; Default project
;;
(global *default-project-directory* "~/cyco-projects")
(global *project-main-filename-format* "~A-main")
(global *default-project-output-directory* "MIDI")
(global *project* nil)
(global *metronome* nil)

(global *config-directory* nil)
