;;;; CYCO3 src/globals
;;;;

(global *os-path-root* "/")
(global *os-path-separator* #\/)
(global *os-extension-separator* #\.)
(global *os-homedir-alias* #\~)

;; Default project
;;
(global *default-project-directory* "~/cyco-projects")
(global *default-project-main-file* "main")
(global *default-project-output-directory* "MIDI")
(global *cyco-config-directory* nil)
(global *cyco-config-file* nil)
(global *project* nil)
(global *metronome* nil)
(global *countin* nil)
(global *endpad* nil)

;; OSC 
;;

(global *local-host* #(127 0 0 1))

(global *default-osc-send-host* *local-host*)
(global *default-osc-send-port* 6800)
(global *default-osc-send-address* "*")

(global *osc-receive-port* *default-osc-send-port*)
(global *osc-receive-address* "CYCO")
(global *osc-receive-buffer-length* 1024)

(global *osc-send-verbose* t)
