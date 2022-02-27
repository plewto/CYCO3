;;;; CYCO generators/bones.lisp
;;;;
;;;; Random number generator
;;;;

(in-package :cyco)

(defclass bones (generator)
  ((random-function
    :type function
    :initform #'(lambda ()(random 1.0))
    :initarg :function)
   (length
    :type integer
    :initform 128
    :initarg :length))
   (:documentation
    "BONES is a random numbergenerator."))

(defun bones (&key (function #'(lambda ()(random 12)))
		   (hook #'(lambda (n) n))
		   (length 128)
		   &allow-other-keys)
  (reset (make-instance 'bones
			:length length
			:hook hook
			:function function
			:seed (funcall function))))

(defmethod reset ((bones bones))
  bones)

(defmethod clone ((mother bones) &key &allow-other-keys)
  (bones :function (slot-value mother 'random-function)
	 :hook (value-hook mother)
	 :length (pattern-length mother)))

(defmethod next-1 ((bones bones))
  (prog1
      (value bones)
    (setf (internal-value bones)
	  (funcall (slot-value bones 'random-function)))))

(defmethod pattern-length ((b bones) &key &allow-other-keys)
  (slot-value b 'length))

(setf (documentation 'bones 'function)
      "Bones is a random-number generator.

(bones &key function hook)

:function - The random number function.  Defaults to (lambda () (random 1.0))
            THE PDF plugin provides several suitable random-number functions.
            
:hook     - The value-hook function. Defaults to (lambda (n) n)")

