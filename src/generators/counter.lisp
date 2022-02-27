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
  (setf (internal-value counter)
	(initial-value counter))
  counter)

(defmethod pattern-length ((c counter) &key &allow-other-keys)
  (let ((diff (abs (- (initial-value c)(final-value c)))))
    (truncate (/ diff (abs (counter-increment c))))))

(defun counter (from to &key (by 1)
		     (hook #'(lambda (n) n))
		     &allow-other-keys)
  (let* ((v0 (truncate from))
	 (v1 (truncate to))
	 (increment (truncate by)))
    (setf increment (if (< v0 v1)
			(abs increment)
		      (* -1 (abs increment))))
    (reset (make-instance 'counter
			  :hook hook
			  :seed v0
			  :from v0
			  :to v1
			  :by increment))))

(defmethod clone ((mother counter) &key &allow-other-keys)
  (counter (initial-value mother)
	   (final-value mother)
	   :by (counter-increment mother)
	   :hook (value-hook mother)))

(labels ((clip (value increment limit)
	       (if (minusp increment)
		   (max value limit)
		 (min value limit))))

  (defmethod next-1 ((counter counter))
    (prog1
	(value counter)
      (let* ((increment (counter-increment counter))
	     (limit (final-value counter))
	     (v0 (internal-value counter))
	     (v1 (clip (+ v0 increment) increment limit)))
	(setf (internal-value counter) v1)))) )

(setf (documentation 'counter 'function)
      "Returns new instance of COUNTER.

(counter from to &key by hook)

from  - Integer, initial value.
to    - Integer, final value.
:by   - Integer, increment.
        The correct increment sign is automatically adjusted based on
        from and to values. Default +1 or -1.
:hook    - Function applied by the value method to the 'natural'
           value of the counter.  Default (lambda (value)) -> value")

