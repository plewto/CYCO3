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
    (setf (internal-value alloy)
	  (funcall (slot-value alloy 'binary-function)
		   (value a)
		   (value b)))
    alloy))

(defun alloy (a b &key (function #'+)
		(hook #'(lambda (n) n))
		&allow-other-keys)
  (reset (make-instance 'alloy :a a :b b
			:hook hook
			:function function)))
		

(defmethod clone ((mother alloy) &key &allow-other-keys)
  (alloy (clone (slot-value mother 'generator-a))
	 (clone (slot-value mother 'generator-b))
	 :function (slot-value mother 'binary-function)
	 :hook (value-hook mother)))

(defmethod next-1 ((alloy alloy))
  (let* ((a (next-1 (slot-value alloy 'generator-a)))
	 (b (next-1 (slot-value alloy 'generator-b)))
	 (fn (slot-value alloy 'binary-function))
	 (c (funcall fn a b)))
    (setf (internal-value alloy) c))
    (value alloy))

(defmethod pattern-length ((alloy alloy) &key (max 128) &allow-other-keys)
  (let ((pa (pattern-length (slot-value alloy 'generator-a) :max max))
	(pb (pattern-length (slot-value alloy 'generator-b) :max max)))
    (if (= pa pb)
	pa
      (* pa pb))))

(setf (documentation 'alloy 'function)
      "Returns new instance of ALLOY

(alloy a b &key function hook)

a - First generator.
b - Second generator.
:function - Binary numeric function, default #'+
:hook     - Hook function applied by the value method.
            (hook (function (value a)(value b)))
            Default (lambda (n) n)")

