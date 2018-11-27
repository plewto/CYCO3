;;;; CYCO Wrapper Pattern
;;;;


(defclass wrapper (pattern)
  ((function
    :type function
    :initform #'identity
    :initarg :function)
   (period
    :type integer
    :initform 16
    :initarg :period))
    (:documentation
     "A WRAPPER is a PATTERN which calls a 'wrapped' function and returns 
it's value."))
   

(defmethod wrapper-p ((obj wrapper)) t)

(defun wrapper (&key (of #'identity)(period 16))
  "Creates new WRAPPER pattern.
:of     - function (lambda n).
          On each call to NEXT-1 the function is called with the internal 
          pointer as an argument, and then the pointer is increment.
          The default function is #'identity
:period - Sets maximum value for internal pointer"
  (make-instance 'wrapper
		 :function of
		 :period (max 1 period)))

(defmethod cardinality ((w wrapper))
  (slot-value w 'period))

(defmethod reset ((w wrapper))
  (setf (value w) nil)
  (setf (pointer w) 0))

(defmethod next-1 ((w wrapper))
  (prog1
      (setf (value w)
	    (funcall (slot-value w 'function)
		     (pointer w)))
    (setf (pointer w)
	  (rem (+ (pointer w) 1)
	       (cardinality w)))))
