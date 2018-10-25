;;;; PigIron cyco coin pattern
;;;;
;;;; A COIN is a PATTERN with binary choice: head or tail.
;;;; The head and tail values my individually be:
;;;;   1) literal value -> return value
;;;;   2) Pattern -> execute next-1 and return result
;;;;   3) Function -> call function with internal counter as argument
;;;;
;;;; Internal counter is reset after period calls.
;;;; The head object is returned with probability p
;;;;
;;;; Note: (retrograde coin) flips head/tail probability.


(defclass coin (pattern)
  ((probability
    :type float
    :accessor probability
    :initform 0.5
    :initarg :p)
   (period
    :type integer
    :initform 0
    :initarg :period)))

(defmethod coin-p ((obj t)) nil)
(defmethod coin-p ((c coin)) t)

(defun coin (&key (p 0.5)(head #'true)(tail #'false)(period 16))
  (make-instance 'coin
		 :p p
		 :of (list head tail)
		 :period (max 1 period)))

(defmethod cardinality ((c coin))
  (slot-value c 'period))

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
    (setf (pointer c)
	  (rem (1+ (pointer c))(cardinality c)))
    (value c)))
