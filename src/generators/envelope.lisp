;;;; CYCO generators/envelopes.lisp
;;;; 
;;;; Defines numeric generators with common envelope contours.
;;;;

(in-package :cyco)

(defclass asr-envelope (generator)
  ((loop-flag
    :type t
    :initform nil
    :initarg :loop
    :documentation
    "If true envelope repeats after final decay value.")
   (attack-increment
    :type number
    :initform 1
    :accessor envelope-attack
    :initarg :attack)
   (decay-decrement
    :type number
    :initform -1
    :accessor envelope-decay
    :initarg :decay)
   (floor
    :type number
    :initform 0
    :accessor envelope-floor
    :initarg :floor
    :documentation
    "The minimum value.")
   (ceiling
    :type number
    :initform 16
    :accessor envelope-ceiling
    :initarg :ceiling
    :documentation
    "The maximum value.")
   (state
    :type keyword
    :initform :attack
    :accessor envelope-state
    :documentation
    "Indicates current envelope stage, may be one of the following
values  :ATTACK  :SUSTAIN or :DECAY")
   (sustain-counter
    :type integer
    :initform 16
    :initarg :sustain)
   (sustain-reset
    :type integer
    :initform 16
    :initarg :sustain))
  (:documentation
   "ASR-ENVELOPE is a three-stage envelope generator."))
    
(defmethod reset ((env asr-envelope))
  (setf (current-value env)(envelope-floor env)
	(slot-value env 'state) :attack
	(slot-value env 'sustain-counter)(slot-value env 'sustain-reset))
  env)


(defun asr-envelope (a b &key
		       (attack 1)
		       (decay 1)
		       (sustain 4)
		       (loop nil)
		       (monitor #'(lambda (envelope value)
				    (declare (ignore envelope value))
				    nil))
		       (action #'(lambda (envelope value)
				   (declare (ignore envelope))
				   value))
		       (hook #'(lambda (n) n)) &allow-other-keys)
  (let ((floor (min a b))
	(ceiling (max a b)))
    (make-instance 'asr-envelope
		   :loop loop
		   :hook hook
		   :monitor monitor
		   :action action
		   :seed floor
		   :attack (abs attack)
		   :decay (* -1 (abs decay))
		   :floor floor
		   :ceiling ceiling
		   :sustain sustain)))


(defmethod clone ((mother asr-envelope) &key &allow-other-keys)
  (asr-envelope (envelope-floor mother)
		(envelope-ceiling mother)
		:attack (envelope-attack mother)
		:decay (envelope-decay mother)
		:sustain (slot-value mother 'sustain-reset)
		:loop (slot-value mother 'loop-flag)
		:hook (value-hook mother)
		:monitor (monitor mother)
		:action (action mother)))

(flet ((next-attack
	(env)
	(let* ((delta (envelope-attack env))
	       (ceiling (envelope-ceiling env))
	       (value (min ceiling (+ (current-value env) delta))))
	  (if (= value ceiling)
	      (setf (envelope-state env) :sustain
		    (slot-value env 'sustain-counter)(slot-value env 'sustain-reset)))
	  (setf (current-value env)
		(if (funcall (monitor env) env value)
		    (funcall (action env) env value)
		  value))))
       
       
       (next-decay
	(env)
	(let* ((delta (envelope-decay env))
	       (floor (envelope-floor env))
	       (value (max floor (+ (current-value env) delta))))
	  (if (= value floor)
	      (setf (envelope-state env)(if (slot-value env 'loop-flag) :attack :off)))
	  (setf (current-value env)
		(if (funcall (monitor env) env value)
		    (funcall (action env) env value)
		  value))))
       

       (next-sustain
	(env)
	(let ((count (slot-value env 'sustain-counter)))
	  (if (not (plusp (- count 2)))
	      (setf (envelope-state env) :decay)
	    (setf (slot-value env 'sustain-counter)(1- count))))) )

  (defmethod next-1 ((env asr-envelope))
    (prog1
	(progn 
	  (if (funcall (monitor env) env (current-value env))
	      (funcall (action env) env))
	(value env))
      (let ((state (envelope-state env)))
	(cond ((eq state :attack)(next-attack env))
	      ((eq state :decay)(next-decay env))
	      ((eq state :sustain)(next-sustain env))
	      (t nil))))))

(setf (documentation 'asr-envelope 'function)
      "Returns new instance of ASR-ENVELOPE.

(asr-envelope a b &key attack decay sustain loop hook monitor action)

a - Number, initial value.
b - Number, peak value.
:attack  - Number, attack stage increment, default 1.
:decay   - Number, decay stage decrement, default 1. 
:sustain - Integer, number of sustain values, default 4
:loop    - Boolean, if true envelope returns to attack stage
           after final decay value.  Default nil
:hook    - Function applied by the value method to the current-value,
           Default (lambda (n) n)
:monitor - Predicate, called within next-1 to determine if action 
           function should be executed.  

           (lambda (envelope value)) --> Boole

           Where envelope may be examined for its current state
           and value is the next output value.  The default always
           returns nil.
:action  - Function executed within next-1 if the monitor function 
           returns non-nil.

           (lambda (envelope value)) --> next-value

           The internal state of envelope may be altered.")
