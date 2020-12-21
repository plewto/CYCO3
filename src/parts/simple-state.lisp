;;;; CYCO parts simple-state.lisp
;;;;
;;;; Defines object for internal state of SIMPLE-PART.
;;;;

(in-package :cyco-part)

(defstruct simple-state
  (source "")
  (time-specification nil)
  (time nil)
  (chord-type '(0))
  (chord-inversion 0)
  (chord-octave 0)
  (articulation 0.0)
  (dynamic 0.5)
  (key nil)
  (pressure nil)
  (controller-number nil)
  (controller-value nil)
  (bend nil)
  (program-number nil)
  (program-bank nil))

(defmethod soft-reset ((state simple-state))
  (setf (simple-state-key state) nil)
  (setf (simple-state-pressure state) nil) 
  (setf (simple-state-controller-number state) nil) 
  (setf (simple-state-controller-value state) nil) 
  (setf (simple-state-bend state) nil) 
  (setf (simple-state-program-number state) nil) 
  (setf (simple-state-program-bank state) nil)
  state)

(defmethod reset ((state simple-state))
  (soft-reset state)
  (setf (simple-state-source state) "")
  (setf (simple-state-time-specification state) nil)
  (setf (simple-state-time state) nil)
  (setf (simple-state-chord-type state) '(0))
  (setf (simple-state-chord-inversion state) 0)
  (setf (simple-state-chord-octave state) 0)
  (setf (simple-state-articulation state) 0.0)
  (setf (simple-state-dynamic state) 0.5)
  state)

(defmethod clone ((mother simple-state) &key new-name new-parent)
  (declare (ignore new-name new-parent))
  (make-simple-state
    :source (simple-state-source mother)
    :time-specification (simple-state-time-specification mother)
    :time (simple-state-time mother)
    :chord-type (simple-state-chord-type mother)
    :chord-inversion (simple-state-chord-inversion mother)
    :chord-octave (simple-state-chord-octave mother)
    :articulation (simple-state-articulation mother)
    :dynamic (simple-state-dynamic mother)
    :key (simple-state-key mother)
    :pressure (simple-state-pressure mother)
    :controller-number (simple-state-controller-number mother)
    :controller-value (simple-state-controller-value mother)
    :bend (simple-state-bend mother)
    :program-number (simple-state-program-number mother)
    :program-bank (simple-state-program-bank mother)))

(defmethod transpose ((state simple-state)(x number))
  (setf (simple-state-key state)
	(transpose (simple-state-key state) x))
  state)
  
(defmethod invert ((state simple-state)(pivot t))
  (if pivot
      (setf (simple-state-key state)
	    (invert (simple-state-key state) pivot)))
  state)
  
