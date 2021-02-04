;;;; CYCO generators/generator.lisp
;;;;


(in-package :cyco)

(defclass generator nil
  ((value-hook
    :type function
    :initform #'(lambda (n) n)
    :accessor value-hook
    :initarg :hook)
   (monitor
    :type function
    :initform #'(lambda (value)(declare (ignore value)) nil)
    :accessor monitor
    :initarg :monitor)
   (action
    :type function
    :initform #'(lambda (generator value)(declare (ignore generator)) value)
    :accessor action
    :initarg :action)
   (current-value
    :type number
    :initform 0
    :accessor current-value
    :initarg :seed))
   
  (:documentation
   "A Generator is a pattern like object for producing numeric sequences.
Unlike true patterns, generators may not contain nested elements, they 
may however be nested within patterns."))
  

(defmethod value ((gen generator))
  (funcall (slot-value gen 'value-hook)
	   (slot-value gen 'current-value)))

(defmethod reset ((gen generator)) gen)

(defmethod next-1 ((gen generator))
  (cyco-error (sformat "NEXT-1 not implemented for Generator class ~A"
		       (class-name (class-of gen)))))

(defmethod next-n ((gen generator)(n integer))
  (loop for i from 1 to n
	collect (next-1 gen)))

(defmethod next ((gen generator) &optional (n 1))
  (cond ((= n 1)(next-1 gen))
	((integerp n)(next-n gen n))
	(t (value gen))))


(defclass constant-value (generator) nil)

(defun constant-value (n &key &allow-other-keys)
  (make-instance 'constant-value :seed n))

(defmethod clone ((mother constant-value) &key &allow-other-keys)
  (constant-value (value mother))) 

(defmethod next-1 ((con constant-value))
  (current-value con))




(setf (documentation 'constant-value 'function)
      "Returns generator with constant value
(param foo (constant-value x))
(next foo) --> x")



