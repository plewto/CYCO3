;;;; CYCO generators/hailstone.lisp
;;;;
;;;; Generates Hailstone sequence
;;;; https://en.wikipedia.org/wiki/Collatz_conjecture
;;;; https://oeis.org/A061641
;;;;

(defclass hailstone-generator (generator)
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

(defmethod reset ((gen hailstone-generator))
  (setf (current-value gen)
	(slot-value gen 'initial-value))
  gen)


(defun hailstone-generator (seed &key
		       (even #'(lambda (n)(/ n 2)))
		       (odd #'(lambda (n)(1+ (* 3 n))))
		       (hook #'(lambda (n) n))
		       &allow-other-keys)
  (reset (make-instance 'hailstone-generator
			:hook hook
			:even even
			:odd odd
			:seed seed)))

(defmethod next-1 ((gen hailstone-generator))
  (prog1
      (value gen)
    (let* ((v0 (current-value gen))
	   (rule (slot-value gen (if (oddp v0) 'odd-rule 'even-rule)))
	   (v1 (funcall rule v0)))
      (setf (current-value gen) v1))))
			
  
;;; TODO implement hailstone-generator clone
;;; TODO add hailstone-generator docstring
