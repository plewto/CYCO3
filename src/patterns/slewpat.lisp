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
   "A Slew is a type of Pattern which delays next-1 calss of an
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
  (setf (pointer s)
	(rem (1+ (pointer s))
	     (cardinality s)))
  (let ((cnt (1- (slot-value s 'slew-counter))))
    (if (not (plusp cnt))
	(progn
	  (setf (slot-value s 'slew-counter)
		(slot-value s 'delay))
	  (next-1 (slot-value s 'client)))
      (setf (slot-value s 'slew-counter) cnt))
    (value (slot-value s 'client))))

(defmethod cardinality ((s slew))
  (* (slot-value s 'delay)
     (cardinality (slot-value s 'client))))

(defmethod elements ((s slew))
  (elements (slot-value s 'client)))

(defmethod transpose ((s slew)(x integer))
  (transpose (slot-value s 'client) x)
  (setf (value s)(transpose (value s) x))
  s)

(defmethod invert ((s slew)(pivot t))
  (invert (slot-value s 'client) pivot)
  (setf (value s)(invert (value s) pivot))
  s)

(defmethod retrograde ((s slew))
  (retrograde (slot-value s 'client))
  s)
	      
(defmethod clone ((mother slew) &key new-name new-parent)
  (dismiss new-name new-parent)
  (slew (clone (slot-value mother 'client))(slot-value mother 'delay)))

