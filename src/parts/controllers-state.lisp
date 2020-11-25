;;;; **** DEPRECIATED ****

(in-package :cyco-part)

(defstruct controllers-state
  (source nil)
  (time-start nil)
  (time-end nil)
  (value-start 0)
  (value-end 127)
  (steps 16)
  (event-type nil))

(defmethod soft-reset ((state controllers-state))
  (setf (controllers-state-event-type state) nil)
  state)

(defmethod reset ((state controllers-state))
  (soft-reset state)
  (setf (controllers-state-source state) nil)
  (setf (controllers-state-time-start state) nil)
  (setf (controllers-state-time-end state) nil)
  (setf (controllers-state-value-start state) 0)
  (setf (controllers-state-value-end state) 127)
  (setf (controllers-state-steps state) 16)
  state)

(defmethod clone ((source controllers-state) &key new-name new-parent)
  (dismiss new-name new-parent)
  (let ((new-state (make-instance 'controllers-state)))
    (setf (controllers-state-source new-state)(controllers-state-source source)
	  (controllers-state-time-start new-state)(controllers-state-time-start source)
	  (controllers-state-time-end new-state)(controllers-state-time-end source)
	  (controllers-state-value-start new-state)(controllers-state-value-start source)
	  (controllers-state-value-end new-state)(controllers-state-value-end source)
	  (controllers-state-steps new-state)(controllers-state-steps source)
	  (controllers-state-event-type new-state)(controllers-state-event-type source))
    new-state))
    
