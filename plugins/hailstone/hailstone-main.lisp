;;;; hailstone-main
;;;;
;;;; CYCO pattern to generate hailstone sequence.
;;;; https://en.wikipedia.org/wiki/Collatz_conjecture
;;;;

(defclass hailstone (pattern)
  ((even-rule
    :type function
    :initform #'(lambda (n)(/ n 2))
    :initarg :even)
   (odd-rule
    :type function
    :initform #'(lambda (n)(1+ (* 3 n)))
    :initarg :odd)
   (value-hook
    :type function
    :initform #'(lambda(n) n)
    :initarg :hook)
   (seed
    :type integer
    :initform 16
    :initarg :seed)
   (current-value
    :type integer
    :initform 16
    :initarg :seed)))

(defgeneric hailstone-p (item))
(defmethod hailstone-p ((item t)) nil)
(defmethod hailstone-p ((item hailstone)) t)

(defmethod reset ((hs hailstone))
  (setf (slot-value hs 'current-value)
	(slot-value hs 'seed)))


(defun hailstone (seed &key
		       (even #'(lambda (n)(/ n 2)))
		       (odd #'(lambda (n)(1+ (* 3 n))))
		       (hook #'(lambda (n) n)))
  (if (and (integerp seed)(plusp seed))
      (make-instance 'hailstone
		     :even even
		     :odd odd
		     :hook hook
		     :seed seed)
    (cyco-value-error 'hailstone seed
		      "Hailstone seed must be positive integer.")))


(defmethod clone ((mother hailstone) &key new-name new-parent)
  (declare (ignore new-name new-parent))
  (hailstone (slot-value mother 'seed)
	     :even (slot-value mother 'even-rule)
	     :odd (slot-value mother 'odd-rule)
	     :hook (slot-value mother 'value-hook)))

(defmethod value ((hs hailstone))
  (funcall (slot-value hs 'value-hook)
	   (slot-value hs 'current-value)))

(defmethod next-1 ((hs hailstone))
  (let* ((v0 (slot-value hs 'current-value))
	 (rule (slot-value hs (if (oddp v0) 'odd-rule 'even-rule)))
	 (v1 (funcall rule v0)))
    (setf (slot-value hs 'current-value) v1)
    (funcall (slot-value hs 'value-hook) v1)))


(defmethod cardinality ((hs hailstone))
  (let* ((working (let ((daughter (clone hs)))
		    (setf (slot-value daughter 'value-hook) #'(lambda (n) n))
		    daughter))
	 (count 0)
	 (current-value (value working)))
    (reset working)
    (while (not (= current-value 1))
      (setf current-value (next-1 working))
      (setf count (1+ count)))
    count))

(flet ((next-all (hs)
		 (let ((n (cardinality hs)))
		   (reset hs)
		   (next-n hs n))))
  (defmethod next ((hs hailstone) &optional (n 1))
    (if (eq n :all)
	(next-all hs)
      (call-next-method))))
    
    
(setf (documentation 'hailstone 'function)
      "Returns hailstone sequence generator.
https://en.wikipedia.org/wiki/Collatz_conjecture

seed  - Initial value
:even - Function for even values v1 = (even v0), default (lambda (n)(/ n 2))
:odd  - Function for odd values  v1 = (odd v0), default (lambda (n)(1+ (* 3 n)))
:hook - Value hook function, default (lambda (n) n)


")
				   
	   
