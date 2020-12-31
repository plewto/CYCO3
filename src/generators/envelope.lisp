;;;; CYCO generators/envelopes.lisp
;;;; 


(in-package :cyco)

(defclass ramp (generator)
  ((floor
    :type number
    :accessor ramp-floor
    :initform 0
    :initarg :floor)
   (ceiling
    :type number
    :accessor ramp-ceiling
    :initform 9
    :initarg :ceiling)
   (increment
    :type number
    :accessor ramp-increment
    :initform 1
    :initarg :increment)))


(defun ramp (a b &key (by 1)(hook #'(lambda (n) n)) &allow-other-keys)
  (make-instance 'ramp
		 :hook hook
		 :seed a
		 :floor a
		 :ceiling b
		 :increment (if (< a b)
				(abs by)
			      (* -1 (abs by)))))

(defmethod reset ((ramp ramp))
  (setf (current-value ramp)(ramp-floor ramp))
  ramp)


(defmethod clone ((mother ramp) &key new-name new-parent)
  (declare (ignore new-name new-parent))
  (let ((daughter (ramp (ramp-floor mother)
			(ramp-ceiling mother)
			:by (ramp-increment mother)
			:hook (value-hook mother))))
    (setf (current-value daughter)(current-value mother))
    daughter))

(flet ((increment (ramp ceiling)
		  (let* ((delta (ramp-increment ramp))
			 (v0 (current-value ramp))
			 (v1 (min ceiling (+ delta v0))))
		    (setf (current-value ramp) v1)))
       
       (decrement (ramp ceiling)
		  (let* ((delta (ramp-increment ramp))
			 (v0 (current-value ramp))
			 (v1 (max ceiling (+ delta v0))))
		    (setf (current-value ramp) v1))) )
  
  (defmethod next-1 ((ramp ramp))
    (prog1
	(value ramp)
      (let ((floor (ramp-floor ramp))
	    (ceiling (ramp-ceiling ramp)))
	(cond ((< floor ceiling)(increment ramp ceiling))
	      ((> floor ceiling)(decrement ramp ceiling))
	      (t nil))))) )

(setf (documentation 'ramp 'function)
      "A RAMP generator creates a linear sequence of numbers.

a     - Initial value
b     - Final value
:by   - Increment. The increment sign is automatically adjusted to match
        the slope determined by a and b.  Default 1 or -1
:hook - Function (lambda (n)) --> n2  applied to 'raw' ramp value.
        Defaults to an identity.

Examples

   (next (ramp 0 9) 12) --> (0 1 2 3 4 5 6 7 8 9 9 9)

   (next (ramp 9 0) 12) --> (9 8 7 6 5 4 3 2 1 0 0 0)

   (next (ramp 0 9 :by 2) 12) --> (0 2 4 6 8 9 9 9 9 9 9 9)

   (next (ramp 9 0 :by 2) 12) --> (9 7 5 3 1 0 0 0 0 0 0 0)

   a, b and by values need not be integers

   (next (ramp 0 9 :by 1.5) 12) --> (0 1.5 3.0 4.5 6.0 7.5 9 9 9 9 9 9)

   Use of hook function

   (next (ramp 0 4 :hook #'(lambda (n)(+ 100 n))) 6) --> (100 101 102 103 104 104)")


(defclass asr-envelope (generator)
  ((cycle-flag
    :type t
    :initform nil
    :initarg :cycle)
   (attack-increment
    :type number
    :initform 1
    :accessor asr-attack
    :initarg :attack)
   (decay-decrement
    :type number
    :initform -1
    :accessor asr-decay
    :initarg :decay)
   (floor
    :type number
    :initform 0
    :accessor asr-floor
    :initarg :floor)
   (ceiling
    :type number
    :initform 16
    :accessor asr-ceiling
    :initarg :ceiling)
   (state
    :type keyword
    :initform :attack
    :accessor asr-state)
   (sustain-counter
    :type integer
    :initform 16
    :initarg :sustain)
   (sustain-reset
    :type integer
    :initform 16
    :initarg :sustain)))
    
(defmethod reset ((env asr-envelope))
  (setf (current-value env)(asr-floor env)
	(slot-value env 'state) :attack
	(slot-value env 'sustain-counter)(slot-value env 'sustain-reset))
  env)


(defun asr-envelope (a b &key (attack 1)(decay 1)(sustain 4)(cycle nil)
		       (hook #'(lambda (n) n)) &allow-other-keys)
  (let ((floor (min a b))
	(ceiling (max a b)))
    (make-instance 'asr-envelope
		   :cycle cycle
		   :hook hook
		   :seed floor
		   :attack (abs attack)
		   :decay (* -1 (abs decay))
		   :floor floor
		   :ceiling ceiling
		   :sustain sustain)))


(defmethod clone ((mother asr-envelope) &key new-name new-parent)
  (declare (ignore new-name new-parent))
  (let ((daughter (asr-envelope (asr-floor mother)
				(asr-ceiling mother)
				:attack (asr-attack mother)
				:decay (asr-decay mother)
				:sustain (slot-value mother 'sustain-reset)
				:cycle (slot-value mother 'cycle-flag)
				:hook (value-hook mother))))
    (setf (current-value daughter)(current-value mother)
	  (asr-state daughter)(asr-state mother)
	  (slot-value daughter 'sustain-counter)(slot-value mother 'sustain-counter))
    daughter))

(flet ((next-attack
	(env)
	(let* ((delta (asr-attack env))
	       (ceiling (asr-ceiling env))
	       (value (min ceiling (+ (current-value env) delta))))
	  (if (= value ceiling)
	      (setf (asr-state env) :sustain
		    (slot-value env 'sustain-counter) (1- (slot-value env 'sustain-reset))))
	  (setf (current-value env) value)))
       
       
       (next-decay
	(env)
	(let* ((delta (asr-decay env))
	       (floor (asr-floor env))
	       (value (max floor (+ (current-value env) delta))))
	  (if (= value floor)
	      (setf (asr-state env)(if (slot-value env 'cycle-flag) :attack :off)))
	  (setf (current-value env) value)))
       

       (next-sustain
	(env)
	(let ((count (slot-value env 'sustain-counter)))
	  (if (plusp count)
	      (setf (asr-state env) :decay)
	    (setf (slot-value env 'sustain-counter)(1- count))))) )

  (defmethod next-1 ((env asr-envelope))
    (prog1 (value env)
      (let ((state (asr-state env)))
	(cond ((eq state :attack)(next-attack env))
	      ((eq state :decay)(next-decay env))
	      ((eq state :sustain)(next-sustain env))
	      (t nil))))))
	     
(setf (documentation 'asr-envelope 'function)
      "The ASR-ENVELOPE generator produces a numeric sequence
with attack, sustain and decay phases.

a        - Initial value.
b        - Peak value, b > a.
:attack  - attack phase increment
:decay   - decay phase decrement
:sustain - number of sustain values.
:cycle   - Boolean, if true envelope returns to attack stage after final 
           decay value has been reached,  Default nil
:hook    - Hook function (lambda (n) ...), defaults to identity.")
