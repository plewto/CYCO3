;;;; CYCO pattern coin.lisp
;;;;
;;;; The COIN pattern produces random binary choices.
;;;;

(in-package :cyco)

(defclass coin (pattern)
  ((probability
    :type float
    :accessor probability
    :initform 0.5
    :initarg :p)
   (length
    :type integer
    :initform 128
    :initarg :length))
  (:documentation 
"A COIN is a PATTERN with binary choice: head or tail.
The head and tail values my individually be:
  1) literal value -> return value.
  2) Pattern -> execute next-1 and return result.
  3) Function -> call function with internal counter as argument.

The head object is returned with probability p"))


(defmethod coin-p ((obj t)) nil)
(defmethod coin-p ((c coin)) t)

(defun coin (&key (p 0.5)(head #'true)(tail #'false)(length 128))
  "Creates new instance of coin pattern."
  (let ((c (make-instance 'coin
			  :p p
			  :of (list head tail)
			  :length length)))
    (next-1 c)
    c))

(defmethod pattern-length ((c coin) &key &allow-other-keys)
  (slot-value c 'length))

(defmethod reset ((c coin))
  (dolist (q (elements c))(reset q))
  (setf (pointer c) 0))

(defmethod next-1 ((c coin))
  (let* ((selection (if (> (probability c)(random 1.0))
			(first (elements c))
		      (second (elements c))))
	 (stype (type-of selection)))
    (setf (value c)
	  (cond ((eq stype 'function)
		 (funcall selection (pointer c)))
		((pattern-p selection)
		 (next-1 selection))
		(t selection)))
    (setf (pointer c)(1+ (pointer c)))
    (value c)))

(defmethod clone ((mother coin) &key &allow-other-keys)
  (coin :p (probability mother)
	:head (clone (car (elements mother)))
	:tail (clone (second (elements mother)))
	:length (pattern-length mother)))

(defmethod retrograde ((c coin)) c)
