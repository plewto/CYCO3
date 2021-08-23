;;;; CYCO pig plugin filter.lisp
;;;;
;;;; CYCO interface to pigiron ChannelFilter
;;;;
;;;; The ChannelIFlter selects MIDI input channels.
;;;;
;;;; q-channels
;;;; select-channels
;;;; select-all-channels
;;;; deselect-all-channels
;;;; invert-channels
;;;;

(in-package :pig)

(global *filter* "filter")

(defun in-channels (&rest channels)
  (osc-send "exec" (sformat "deselect-all-channels ~A" *filter*))
  (sleep 0.1)
  (dolist (c channels)
    (osc-send "exec" (sformat "select-channel, ~A, ~A" *filter* c))
    (sleep 0.1)))

