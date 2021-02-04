;;;; CYCO generators/hailstone.lisp
;;;;
;;;; Generates Hailstone sequence
;;;; https://en.wikipedia.org/wiki/Collatz_conjecture
;;;; https://oeis.org/A061641
;;;;

(in-package :cyco)

(defclass hailstone (generator)
  ((even-rule
    :type function
    :initform #'(lambda (n)(/ n 2))
    :initarg :even)
   (odd-rule
    :type function
    :initform #'(lambda (n)(1+ (* 3 n)))
    :initarg :odd)
   (initial-value
    :type integer
    :initform 128
    :initarg :seed)))

(defmethod reset ((gen hailstone))
  (setf (current-value gen)
	(slot-value gen 'initial-value))
  gen)


(defun hailstone (seed &key (even #'(lambda (n)(/ n 2)))
		       (odd #'(lambda (n)(1+ (* 3 n))))
		       (hook #'(lambda (n) n))
		       (monitor #'(lambda (value)
				    (declare (ignore value))
				    nil))
		       (action #'(lambda (hailstone value)
				   (declare (ignore hailstone))
				   value))
		       &allow-other-keys)
  (reset (make-instance 'hailstone
			:hook hook
			:even even
			:odd odd
			:monitor monitor
			:action action
			:seed seed)))

(defmethod clone ((mother hailstone) &key &allow-other-keys)
  (hailstone (slot-value mother 'initial-value)
	     :even (slot-value mother 'even-rule)
	     :odd (slot-value mother 'odd-rule)
	     :monitor (monitor mother)
	     :action (action mother)
	     :hook (value-hook mother)))

(defmethod next-1 ((gen hailstone))
  (prog1
      (value gen)
    (let* ((v0 (current-value gen))
	   (rule (slot-value gen (if (oddp v0) 'odd-rule 'even-rule)))
	   (v1 (funcall rule v0)))
      (setf (current-value gen)
	    (if (funcall (monitor gen) v1)
		(funcall (action gen) gen v1)
	      v1)))))

;; TODO update hailstone docstring

(setf (documentation 'hailstone 'function)
      "Returns generator that produces a hailstone sequence.
https://en.wikipedia.org/wiki/Collatz_conjecture
https://oeis.org/A061641

seed  - Integer, starting value. 
:even - Function applied for even result, default (lambda (n)(* 2 n))
:odd  - Function applied for odd result, default (lambda (n)(1+ (* 3 n)))
:hook - Function applied to value, default (lambda (n) n)")
