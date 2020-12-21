;;;; CYCO parts controllers-state.lisp
;;;;
;;;; Internal render state for controller class.
;;;; Doubles as state for bender class.
;;;;

(in-package :cyco-part)

(defstruct controllers-state
  (source "")
  (start-time nil)
  (end-time nil)
  (time-interval 0.1)
  (start-value 0)
  (end-value 127)
  (controller 1)
  (cycles 1)
  (phase 0)
  (width 0.5)
  (curve nil)
  (single-event nil))


(defmethod soft-reset ((state controllers-state))
  (setf (controllers-state-curve state) nil)
  (setf (controllers-state-single-event state) nil)
  state)

(defmethod reset ((state controllers-state))
  (soft-reset state)
  (setf (controllers-state-start-time state) nil)
  (setf (controllers-state-end-time state) nil)
  (setf (controllers-state-time-interval state) 0.1)
  (setf (controllers-state-start-value state) 0)
  (setf (controllers-state-end-value state) 127)
  (setf (controllers-state-controller state) 1)
  (setf (controllers-state-cycles state) 1)
  (setf (controllers-state-phase state) 0)
  (setf (controllers-state-width state) 0.5)
  state)


(defmethod clone ((mother controllers-state) &key new-name new-parent)
  (declare (ignore new-name new-parent))
  (make-controllers-state
   :source (controllers-state-source mother)
   :start-time (controllers-state-start-time mother)
   :end-time (controllers-state-end-time mother)
   :time-interval (controllers-state-time-interval mother)
   :start-value (controllers-state-start-value mother)
   :end-value (controllers-state-end-value mother)
   :controller (controllers-state-controller mother)
   :cycles (controllers-state-cycles mother)
   :phase (controllers-state-phase mother)
   :width (controllers-state-width mother)
   :curve (controllers-state-curve mother)
   :single-event (controllers-state-single-event mother)))
   


