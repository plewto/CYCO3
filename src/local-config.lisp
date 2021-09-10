;;;; cyco local-config.lisp
;;;;
;;;; Modifies plugins search path.
;;;; 

(in-package :cyco)

(setf *os-path-root* "/")
(setf *os-path-separator* #\/)
(setf *os-extension-separator* #\.)
(setf *os-homedir-alias* #\~)


;; Default directories
;;
(setf *cyco-location* "~/cyco3")
(setf *config-directory* "~/.config/cyco")
(setf *projects-root* "~/cyco-projects")
(setf *project-main-filename-format* "~A-main")
(setf *default-project-output-directory* "MIDI")

;; plugin path
;;
(push-plugin-search-path (join-path *cyco-location* "plugins"))
(push-plugin-search-path (join-path *config-directory* "plugins"))

(setf *ticks-per-beat* 480)
