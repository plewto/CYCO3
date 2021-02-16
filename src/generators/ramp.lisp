;;;; CYCO generators/ramp.lisp
;;;;
;;;;
;;;; Generator for arithmetic sequence.  Ramps are similar to
;;;; counters except they may have non-integer values.
;;;; The other major difference is that the action function
;;;; for a counter may modify the sequence of values.  Self
;;;; modification is more convoluted for ramps.

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

(defun ramp (a b &key (by 1)
	       (hook #'(lambda (n) n))
	       (monitor #'(lambda (v)
			    (declare (ignore v))
			    nil))
	       (action #'(lambda (ramp)
			   (declare (ignore ramp))))
	       &allow-other-keys)
  (make-instance 'ramp
		 :hook hook
		 :monitor monitor
		 :action action 
		 :seed a
		 :floor a
		 :ceiling b
		 :increment (if (< a b)
				(abs by)
			      (* -1 (abs by)))))

(defmethod reset ((ramp ramp))
  (setf (current-value ramp)(ramp-floor ramp))
  ramp)

(defmethod clone ((mother ramp) &key &allow-other-keys)
  (ramp (ramp-floor mother)
	(ramp-ceiling mother)
	:by (ramp-increment mother)
	:monitor (monitor mother)
	:action (action mother)
	:hook (value-hook mother)))


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
	(progn
	  (if (funcall (monitor ramp)(current-value ramp))
	      (funcall (action ramp) ramp))
	  (value ramp))
      (let ((floor (ramp-floor ramp))
	    (ceiling (ramp-ceiling ramp)))
	(cond ((< floor ceiling)(increment ramp ceiling))
	      ((> floor ceiling)(decrement ramp ceiling))
	      (t nil))))) )

(defmethod pattern-length ((r ramp) &key &allow-other-keys)
  (let ((diff (abs (- (ramp-ceiling r)(ramp-floor r)))))
    (truncate (/ diff (ramp-increment r)))))

(setf (documentation 'ramp 'function)
      "Creates new RAMP generator.

(ramp a b &key by hook monitor action)

a - Number, initial value
b - Number, final value
:by      - Increment, default -1 or +1
:hook    - Function applied by the value method to the internal value.
           Default (lambda (n) n)
:monitor - Predicate called within next-1 to determine if action function 
           should be executed.  Default (lambda (value) nil)
:action  - Function called with next-1 whenever the monitor predicate
           returns non-nil.  Default (lambda (ramp))")
