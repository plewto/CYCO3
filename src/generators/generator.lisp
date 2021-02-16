;;;; CYCO generators/generator.lisp
;;;;

(in-package :cyco)

(defclass generator nil
  ((value-hook
    :type function
    :initform #'(lambda (n) n)
    :accessor value-hook
    :initarg :hook
    :documentation
    "Each instance of Generator has an 'internal-value'.  When the 
value method is called, the internal-value is passed as an argument to 
the value-hook function.   The result of value-hook then becomes the 
return for the value method.  The default value-hook is an identity
(lambda (n) n)")
   (monitor
    :type function
    :initform #'(lambda (value)(declare (ignore value)) nil)
    :accessor monitor
    :initarg :monitor
    :documentation
    "Whenever the next-1 method is called, the new internal-value is
passed to the monitor function.  If the monitor returns non-nil the 
action function is executed.   The default monitor returns a constant
nil  (lambda (value) nil)")
   (action
    :type function
    :initform #'(lambda (generator value)(declare (ignore generator)) value)
    :accessor action
    :initarg :action
    :documentation
    "If the monitor function returns non-nil within a call to next-1, 
the action function is executed.  The exact form of the action function 
is different for specific generator sub classes.   In most cases it is
able to alter the internal state of the generator.")
   (internal-value
    :type number
    :initform 0
    :accessor internal-value
    :initarg :seed))
  (:documentation
   "A GENERATOR is a pattern like object for producing numeric sequences.
Unlike true patterns, generators may not contain nested elements, they 
may however be nested within patterns.

Each Generator instance has associated monitor and action functions.
When the next-1 method is called, the monitor function looks at the 
new value and returns either nil or t.  If the result is t, the 
action function is executed."))
  
(defmethod value ((gen generator))
  (funcall (slot-value gen 'value-hook)
	   (slot-value gen 'internal-value)))

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

(defclass constant-value (generator) nil
  (:documentation
   "A CONSTANT-VALUE is a generator which always produces the same 
value."))

(defun constant-value (n &key &allow-other-keys)
  (make-instance 'constant-value :seed n))

(defmethod pattern-length ((c constant-value) &key &allow-other-keys) 1)

(defmethod clone ((mother constant-value) &key &allow-other-keys)
  (constant-value (value mother))) 

(defmethod next-1 ((con constant-value))
  (internal-value con))

(setf (documentation 'constant-value 'function)
      "Returns generator with constant value.
(param foo (constant-value x))

Where x is a numeric-value.

(next foo) --> x")
