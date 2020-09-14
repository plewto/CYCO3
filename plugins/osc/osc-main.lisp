;;;; CYCO OSC plugin
;;;;

(load "~/quicklisp/setup")
(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :osc)
  (ql:quickload :usocket))

(import '(osc:encode-message
	  usocket:socket-connect
	  usocket:socket-send
	  usocket:socket-receive
	  usocket:socket-close)
	:cyco)

(global *local-host* #(127 0 0 1))
(global *default-osc-send-host* *local-host*)
(global *default-osc-send-port* 6000)
(global *default-osc-send-address* "pig/op/MidiPlayer")
;; (global *osc-receive-port* *default-osc-send-port*)
;; (global *osc-receive-address* "CYCO")
;; (global *osc-receive-buffer-length* 1024)
(global *osc-send-verbose* t)

(load-plugin-file "send")
;; (load-plugin-file "receive")


