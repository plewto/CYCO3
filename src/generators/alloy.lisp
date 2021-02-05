;;;; CYCO generators/alloy
;;;;
;;;; Combines two generators.
;;;;

(in-package :cyco)

(defclass alloy (generator)
  ((generator-a
    :type generator
    :initform (constant-value 0)
    :initarg :a)
   (generator-b
    :type generator
    :initform (constant-value 1)
    :initarg :b)
   (binary-function
    :type function
    :initform #'+
    :initarg :function))
  (:documentation
   "An ALLOY is a generator which combines the outputs of two 
embedded generators using a binary function.  The default 
function is addition."))

(defmethod reset ((alloy alloy))
  (let ((a (reset (slot-value alloy 'generator-a)))
	(b (reset (slot-value alloy 'generator-b))))
    (setf (current-value alloy)
	  (funcall (slot-value alloy 'binary-function)
		   (value a)
		   (value b)))
    alloy))

(defun alloy (a b &key (function #'+)
		(hook #'(lambda (n) n))
		(monitor #'(lambda (value)
			     (declare (ignore value))
			     nil))
		(action #'(lambda (alloy value)
			    (declare (ignore alloy))
			    value))
		&allow-other-keys)
  (reset (make-instance 'alloy :a a :b b
			:hook hook
			:function function
			:monitor monitor
			:action action)))

(defmethod clone ((mother alloy) &key &allow-other-keys)
  (alloy (clone (slot-value mother 'generator-a))
	 (clone (slot-value mother 'generator-b))
	 :function (slot-value mother 'binary-function)
	 :hook (value-hook mother)
	 :monitor (monitor mother)
	 :action (action mother)))

(defmethod next-1 ((alloy alloy))
  (let* ((a (next-1 (slot-value alloy 'generator-a)))
	 (b (next-1 (slot-value alloy 'generator-b)))
	 (fn (slot-value alloy 'binary-function))
	 (c (funcall fn a b)))
    (setf (current-value alloy)
	  (if (funcall (monitor alloy) c)
	      (funcall (action alloy) alloy c)
	    c))
    (value alloy)))

(setf (documentation 'alloy 'function)
      "Returns new instance of ALLOY

(alloy a b &key function hook monitor action)

a - First generator.
b - Second generator.
:function - Binary numeric function, default #'+
:hook     - Hook function applied by the value method.
            (hook (function (value a)(value b)))
            Default (lambda (n) n)
:monitor  - Value monitor predicate, (lambda (value)) --> boolean
:action   - Action function executed whenever monitor returns true
            within next-1.   

            (lambda (alloy next-value-1)) --> next-value-2

            The nominal next-value is passed as an argument,
            the return becomes the actual next-value.  The alloy
            argument may be used to alter the internal state 
            of alloy.

            Default (lambda (alloy value)) --> value.")
