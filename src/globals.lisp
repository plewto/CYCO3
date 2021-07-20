;;;; CYCO globals.lisp
;;;;
;;;; Defines global variables.

(in-package :cyco)

(global *os-path-root* "/")
(global *os-path-separator* #\/)
(global *os-extension-separator* #\.)
(global *os-homedir-alias* #\~)

;; Default directories
;;
(global *cyco-location* nil)
(global *config-directory* nil)
(global *projects-root* "~/cyco-projects")
(global *project-main-filename-format* "~A-main")
(global *default-project-output-directory* "MIDI")

(global *project* nil)
(global *metronome* nil)

(global *ticks-per-beat* 480)
