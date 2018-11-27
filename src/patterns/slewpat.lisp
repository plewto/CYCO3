;;;; CYCO slew Pattern
;;;; 

(defclass slew (pattern)
  ((client 
    :type pattern
    :initarg :client)
   (delay
    :type integer
    :initform 3
    :initarg :delay)
   (slew-counter
    :type integer
    :initarg :delay))
  (:documentation
   "A Slew is a type of Pattern which delays calls to next-1 for an
embedded pattern.

(slew (cycle :of '(A B C) 3)) produces the same results as 
(cycle :of '(A A A B B B C C C))"))

    
(defmethod reset ((s slew))
  (next-1 (reset (slot-value s 'client)))
  (setf (slot-value s 'slew-counter)
	(1+ (slot-value s 'delay)))
  s)

(defun slew (pattern delay)
  (reset (make-instance 'slew
			:client pattern
			:delay delay)))

(defmethod value ((s slew))
  (value (slot-value s 'client)))

(defmethod next-1 ((s slew))
  (let ((cnt (1- (slot-value s 'slew-counter))))
    (if (not (plusp cnt))
	(progn
	  (setf (slot-value s 'slew-counter)
		(slot-value s 'delay))
	  (next-1 (slot-value s 'client)))
      (setf (slot-value s 'slew-counter) cnt))
    (value (slot-value s 'client))))


