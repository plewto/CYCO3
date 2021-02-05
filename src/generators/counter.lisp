;;;; CYCO generators/ramp.lisp
;;;;
;;;; Integer arithmetic-sequence generator.

(in-package :cyco)

(defclass counter (generator)
  ((initial-value
    :type integer
    :initform 0
    :accessor initial-value
    :initarg :from)
   (final-value
    :type integer
    :initform 9
    :accessor final-value
    :initarg :to)
   (increment
    :type integer
    :initform 1
    :accessor counter-increment
    :initarg :by))
  (:documentation
   "COUNTER is an integer arithmetic-sequence generator."))

(defmethod reset ((counter counter))
  (setf (current-value counter)
	(initial-value counter))
  counter)


(defun counter (from to &key (by 1)
		     (hook #'(lambda (n) n))
		     (monitor #'(lambda (value)
				  (declare (ignore value))
				  nil))
		     (action #'(lambda (generator value)
				 (declare (ignore generator))
					  value))
		     &allow-other-keys)
  (let* ((v0 (truncate from))
	 (v1 (truncate to))
	 (increment (truncate by)))
    (setf increment (if (< v0 v1)
			(abs increment)
		      (* -1 (abs increment))))
    (reset (make-instance 'counter
			  :hook hook
			  :monitor monitor
			  :action action
			  :seed v0
			  :from v0
			  :to v1
			  :by increment))))

(defmethod clone ((mother counter) &key &allow-other-keys)
  (counter (initial-value mother)
	   (final-value mother)
	   :by (counter-increment mother)
	   :hook (value-hook mother)
	   :monitor (monitor mother)
	   :action (action mother)))


(labels ((clip (value increment limit)
	       (if (minusp increment)
		   (max value limit)
		 (min value limit))))

  (defmethod next-1 ((counter counter))
    (prog1
	(value counter)
      (let* ((increment (counter-increment counter))
	     (limit (final-value counter))
	     (v0 (current-value counter))
	     (v1 (clip (+ v0 increment) increment limit)))
	(setf (current-value counter)
	      (if (funcall (monitor counter) v1)
		  (funcall (action counter) counter v1)
		v1))))) )

(setf (documentation 'counter 'function)
      "Returns new instance of COUNTER.

(counter from to &key by hook monitor action)

from  - Integer, initial value.
to    - Integer, final value.
:by   - Integer, increment.
        The correct increment sign is automatically adjusted based on
        from and to values. Default +1 or -1.
:hook    - Function applied by the value method to the 'natural'
           value of the counter.  Default (lambda (value)) -> value
:monitor - Predicate called by next-1 to determine if action function
           should be executed.  Default (lambda (value)) -> Boole.
:action  - Function executed within next-1 whenever monitor returns
           non-nil.  The action return becomes the next current-value
           Default (lambda (counter value)) -> value.
           The counter argument may be used to alter the internal
           state of the counter.")
