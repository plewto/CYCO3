;;;; CYCO pig plugin distributor.lisp
;;;;
;;;; CYCO interface to pigiron Distributor
;;;;
;;;; The ChannelIFlter selects MIDI output channels
;;;;
;;;; q-channels
;;;; select-channels
;;;; select-all-channels
;;;; deselect-all-channels
;;;; invert-channels
;;;;

(in-package :pig)

(global *distributor* "dist")

(defun out-channels (&rest channels)
  (osc-send "exec" (sformat "deselect-all-channels ~A" *distributor*))
  (sleep 0.1)
  (dolist (c channels)
    (osc-send "exec" (sformat "select-channel, ~A, ~A" *distributor* c))
    (sleep 0.1)))

