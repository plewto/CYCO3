;;;; CYCO generators/bones.lisp
;;;;
;;;; Random number generator
;;;;

(in-package :cyco)

(defclass bones (generator)
  ((random-function
    :type function
    :initform #'(lambda ()(random 1.0))
    :initarg :function))
   (:documentation
    "BONES is a random numbergenerator."))

(defun bones (&key (function #'(lambda ()(random 12)))
		   (hook #'(lambda (n) n))
		   (monitor #'(lambda (value)
				(declare (ignore value))
				nil))
		   (action #'(lambda ()))
		   &allow-other-keys)
  (reset (make-instance 'bones
			:hook hook
			:monitor monitor
			:action action
			:function function
			:seed (funcall function))))

(defmethod reset ((bones bones))
  bones)

(defmethod clone ((mother bones) &key &allow-other-keys)
  (bones :function (slot-value mother 'random-function)
	 :hook (value-hook mother)
	 :monitor (monitor mother)
	 :action (action mother)))

(defmethod next-1 ((bones bones))
  (prog1
      (progn
	(if (funcall (monitor bones)(current-value bones))
	    (funcall (action bones)))
      (value bones))
    (setf (current-value bones)
	  (funcall (slot-value bones 'random-function)))))

(setf (documentation 'bones 'function)
      "Bones is a random-number generator.

(bones &key function hook monitor action)

:function - The random number function.  Defaults to (lambda () (random 1.0))
            THE PDF plugin provides several suitable random-number functions.
            
:hook     - The value-hook function. Defaults to (lambda (n) n)
:monitor  - Predicate, called within next-1 to trigger the action 
            function.  Default (lambda (value)) -> nil
:action   - Function executed whenever the monitor returns non-nil within 
            a next-1 call.   Action is used solely for its side effects.

            Default (lambda ())")
