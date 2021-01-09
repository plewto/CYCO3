;;;; CYCO generators/hailstone.lisp
;;;;
;;;; Generates Hailstone sequence
;;;; https://en.wikipedia.org/wiki/Collatz_conjecture
;;;; https://oeis.org/A061641
;;;;

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


(defun hailstone (seed &key
				 (even #'(lambda (n)(/ n 2)))
				 (odd #'(lambda (n)(1+ (* 3 n))))
				 (hook #'(lambda (n) n))
				 &allow-other-keys)
  (reset (make-instance 'hailstone
			:hook hook
			:even even
			:odd odd
			:seed seed)))

(defmethod clone ((mother hailstone) &key new-name new-parent)
  (declare (ignore new-name new-parent))
  (let ((daughter (hailstone (slot-value mother 'initial-value)
				       :even (slot-value mother 'even-rule)
				       :odd (slot-value mother 'odd-rule)
				       :hook (value-hook mother))))
    (setf (current-value daughter)
	  (current-value mother))
    daughter))

(defmethod next-1 ((gen hailstone))
  (prog1
      (value gen)
    (let* ((v0 (current-value gen))
	   (rule (slot-value gen (if (oddp v0) 'odd-rule 'even-rule)))
	   (v1 (funcall rule v0)))
      (setf (current-value gen) v1))))
			
(setf (documentation 'hailstone 'function)
      "Returns generator that produces a hailstone sequence.
https://en.wikipedia.org/wiki/Collatz_conjecture
https://oeis.org/A061641

seed  - Integer, starting value. 
:even - Function applied for even result, default (lambda (n)(* 2 n))
:odd  - Function applied for odd result, default (lambda (n)(1+ (* 3 n)))
:hook - Function applied to value, default (lambda (n) n)")
