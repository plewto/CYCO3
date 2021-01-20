;;;; CYCO generators/bones.lisp
;;;;
;;;; Random number generator
;;;;

(in-package :cyco)

(defclass bones (generator)
  ((random-function
    :type function
    :initform #'(lambda ()(random 1.0))
    :initarg :function)))

(defun bones (&key (function #'(lambda ()(random 1.0)))(hook #'(lambda (n) n)) &allow-other-keys)
  (reset (make-instance 'bones
			:hook hook
			:function function
			:seed (funcall function))))

(defmethod reset ((bones bones))
  bones)

(defmethod clone ((mother bones) &key new-name new-parent)
  (declare (ignore new-name new-parent))
  (let ((daughter (bones :function (slot-value mother 'random-function)
			 :hook (value-hook mother))))
    (reset daughter)))

(defmethod next-1 ((bones bones))
  (prog1
      (value bones)
    (setf (current-value bones)
	  (funcall (slot-value bones 'random-function)))))

(setf (documentation 'bones 'function)
      "Bones is a random-number generator.

:function - The random number function.  Defaults to (lambda () (random 1.0))
:hook     - The value-hook function. Defaults to (lambda (n) n)

The PDF plugin provides several random-number generators suitable for use with bones.")
