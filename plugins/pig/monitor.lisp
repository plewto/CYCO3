;;;; CYCO pig plugin monitor.lisp
;;;;
;;;; CYCO interface to Pigiron Monitor operator.
;;;;

(in-package :pig)

(global *monitor* "mon")


(defun monitor-exclude (status flag)
  (osc-send "exec" (sformat "op ~A, exclude-status, ~D, ~A" *monitor* status (bool->str flag))))

(defun monitor-on ()
  (osc-send "exec" (sformat "op ~A, enable, ~A" *monitor* "true")))

(defun monitor-off ()
  (osc-send "exec" (sformat "op ~A, enable, ~A" *monitor* "true")))

