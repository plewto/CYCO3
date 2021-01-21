;;;; CYCO pattern wrapper.lisp
;;;;
;;;; The WRAPPER pattern allows a function to be treated as a pattern.
;;;;

(in-package :cyco)

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
its value."))
   

(defmethod wrapper-p ((obj wrapper)) t)

(defun wrapper (&key (of #'identity)(period 16))
  "Creates new WRAPPER pattern.
:of     - function (lambda n).
          On each call to NEXT-1 the function is called with the internal 
          pointer as an argument, the pointer is then incremented.
          The default function is #'identity
:period - Sets maximum value for internal pointer"
  (let ((w (make-instance 'wrapper
			  :function of
			  :period (max 1 period))))
    (setf (value w)(funcall of 0))
    w))

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

(defmethod clone ((mother wrapper) &key &allow-other-keys)
  (wrapper :of (slot-value mother 'function)
	   :period (slot-value mother 'period)))
