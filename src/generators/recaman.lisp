;;;; CYCO generators/recaman
;;;;
;;;; Recaman sequence generator.
;;;; https://en.wikipedia.org/wiki/Recam%C3%A1n%27s_sequence
;;;; https://oeis.org/A005132
;;;;

(in-package :cyco)

(defclass recaman (generator)
  ((seen
    :type list
    :initform '())
   (initial-value
    :type integer
    :initform 16
    :initarg :seed)
   (counter
    :type integer
    :initform 0)))

(defmethod reset ((rec recaman))
  (setf (current-value rec)
	(slot-value rec 'initial-value)
	(slot-value rec 'seen) '()
	(slot-value rec 'counter) 0)
  rec)

(defun recaman (seed &key
		     (monitor #'(lambda (value)
				  (declare (ignore value))
				  nil))
		     (action #'(lambda (value) value))
		     (hook #'(lambda (n) n)) &allow-other-keys)
  (reset (make-instance 'recaman
			:seed seed 
			:hook hook 
			:monitor monitor 
			:action action)))

(defmethod clone ((mother recaman) &key &allow-other-keys)
  (recaman (slot-value mother 'initial-value)
	   :hook (value-hook mother)
	   :monitor (monitor mother)
	   :action (action mother)))

(defmethod next-1 ((rec recaman))
  (prog1
      (value rec)
    (let* ((v0 (current-value rec))
	   (n (1+ (slot-value rec 'counter)))
	   (v1 (- v0 n)))
      (setf v1 (cond ((zerop n) 1)
		     ((and (plusp v1)
			   (not (member v1 (slot-value rec 'seen))))
		      v1)
		     (t (+ v0 n))))
      (if (funcall (monitor rec) v1)
	  (setf v1 (funcall (action rec) v1)))
      (push v1 (slot-value rec 'seen))
      (setf (slot-value rec 'counter) n
	    (current-value rec) v1))))

;; TODO update recaman docstring

(setf (documentation 'recaman 'function)
      "Returns generator that produces the Recaman sequence.
https://en.wikipedia.org/wiki/Recam%C3%A1n%27s_sequence
https://oeis.org/A005132

seed  - Integer, initial value. 
:hook - Value hook function, default (lambda (n) n)")


