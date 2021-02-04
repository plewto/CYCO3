;;;; CYCO generators/ramp.lisp
;;;; 

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
    :initarg :by)))


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


;;; TODO add counter docstring
