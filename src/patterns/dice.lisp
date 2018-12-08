;;;; CYCO Dice Pattern
;;;;

(defclass dice (pattern) nil
  (:documentation
   "A DICE is a PATTERN where elements are drawn at random with 
replacement."))

(defmethod dice-p ((obj dice)) obj)

(defun dice (&key (of '()))
  (let ((d (reset (make-instance 'dice :of (->list of)))))
    (setf (value d)(value (pick of)))
    d))

(defmethod next-1 ((q dice))
  (setf (pointer q)
	(rem (1+ (pointer q))(cardinality q)))
  (setf (value q)(next-1 (pick (elements q)))))



