;;;; CYCO generators/hailstone.lisp
;;;;
;;;; Generates Hailstone sequence.
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
    :initarg :seed))
  (:documentation
   "HAILSTONE generates values related to the 
    Collatz conjecture."))

(defmethod reset ((gen hailstone))
  (setf (current-value gen)
	(slot-value gen 'initial-value))
  gen)

(defun hailstone (seed &key
		       (even #'(lambda (n)(/ n 2)))
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

(labels ((whole-number-p (n)
			 (zerop (- n (truncate n))))

	 (power-of-2-p (n)
		       (whole-number-p (log n 2))))

  (defmethod pattern-length ((stone hailstone) &key &allow-other-keys)
    (reset stone)
    (let ((counter 0))
      (while (not (power-of-2-p (current-value stone)))
	(next-1 stone)
	(setf counter (1+ counter)))
      (reset stone)
      (+ 1 counter (truncate (log (current-value stone) 2))))))
  


(setf (documentation 'hailstone 'function)
      "Returns new instance of HAILSTONE.

(hailstone seed &key event odd hook monitor action)

seed  - Integer, initial value.
:even - Function applied for even values,
        Default (lambda (n)(/ n 2))
:odd  - Function applied to odd values.
        Default (lambda (n)(1+ (* 3 n)))
:hook    - Function applied by VALUE method on the internal value.
           Default (lambda (n) n)   
:monitor - Predicate called within next-1 to determine if action
           function should be executed.  
           Default (lambda (value)) -> nil
:action  - Function executed within next-1 whenever monitor returns 
           non-nil.  Default (lambda (hailstone value)) -> value")
           
