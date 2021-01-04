;;;; CYCO generators/generator.lisp
;;;;


(in-package :cyco)

(defclass generator nil
  ((value-hook
    :type function
    :initform #'(lambda (n) n)
    :accessor value-hook
    :initarg :hook)
   (current-value
    :type number
    :initform 0
    :accessor current-value
    :initarg :seed)))
  

(defmethod value ((gen generator))
  (funcall (slot-value gen 'value-hook)
	   (slot-value gen 'current-value)))

(defmethod reset ((gen generator)) gen)

(defmethod next-1 ((gen generator)) (value gen))


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

(defmethod clone ((mother constant-value) &key new-name new-parent)
  (declare (ignore new-name new-parent))
  (constant-value (value mother))) 

