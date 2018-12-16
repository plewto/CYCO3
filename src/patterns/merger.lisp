;;;; CYCO Merger Pattern
;;;;

(defclass merger (pattern)
  ((function-a
    :type function
    :accessor merger-function-a
    :initform #'identity
    :initarg :a-function)
   (function-b
    :type function
    :accessor merger-function-b
    :initform #'identity
    :initarg :b-function)
   (mixer
    :type function
    :accessor merger-mixer-function
    :initform #'(lambda (a b)(cons a b))
    :initarg :mixer-function))
  (:documentation
   "A MERGER combines two Pattern objects a & b.
Upon calling NEXT-1 the next values of patterns a and b are generated.
These are processed by the a-function and b-function functions respectively.  a-function
and b-function take a single argument and have an undefined return type.  The
results of a-function and b-function are combined by the mixer function.
mixer takes tow arguments and has an undefined return type."))

(defun merger (a b &key
		 (a-function #'identity)
		 (b-function #'identity)
		 (mixer-function #'(lambda (a b)(cons a b))))
  (reset (make-instance 'merger :of (list a b)
			:a-function a-function
			:b-function b-function
			:mixer-function mixer-function)))

(defmethod value ((m merger))
  (let ((a (car (elements m)))
	(b (second (elements m))))
    (funcall (merger-mixer-function m)
	     (funcall (merger-function-a m)(value a))
	     (funcall (merger-function-b m)(value b)))))
	     
(defmethod reset ((m merger))
  (reset (car (elements m)))
  (reset (second (elements m)))
  m)

(defmethod cardinality ((m merger))
  (let ((a (cardinality (car (elements m))))
	(b (cardinality (second (elements m)))))
    (if (= a b) a (* a b))))

(defmethod clone ((m merger) &key new-name new-parent)
  (dismiss new-name new-parent)
  (let* ((e (elements m))
	 (a (clone (car e)))
	 (b (clone (second e))))
    (merger a b
	    :a-function (merger-function-a m)
	    :b-function (merger-function-b m)
	    :mixer-function (merger-mixer-function m))))

(defmethod next-1 ((m merger))
  (setf (pointer m)
	(rem (1+ (pointer m))
	     (cardinality m)))
  (next-1 (car (elements m)))
  (next-1 (second (elements m)))
  (value m))

(defmethod retrograde ((m merger))
  (dolist (obj (elements m))
    (retrograde obj))
  m)
