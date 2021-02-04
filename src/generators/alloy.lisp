;;;; CYCO generators/alloy
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
    :initarg :function)))


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
  (prog1
      (value alloy)
    (let* ((a (next-1 (slot-value alloy 'generator-a)))
	   (b (next-1 (slot-value alloy 'generator-b)))
	   (fn (slot-value alloy 'binary-function))
	   (c (funcall fn a b)))
      (setf (current-value alloy)
	    (if (funcall (monitor alloy) c)
		(funcall (action alloy) alloy c)
	      c)))))

;; TODO add alloy documentation
;;

