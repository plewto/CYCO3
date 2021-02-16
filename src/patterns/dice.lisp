;;;; CYCO pattern dice.lisp
;;;;
;;;; The DICE pattern generates random values with replacement.
;;;;

(in-package :cyco)

(defclass dice (pattern)
  ((length
    :type integer
    :initform 128
    :initarg :length))
  (:documentation
   "A DICE is a PATTERN where elements are drawn at random with 
replacement."))

(defmethod dice-p ((obj dice)) obj)

(defun dice (&key (of '())(length 128))
  "Creates new instance of dice pattern."
  (let ((d (reset (make-instance 'dice :of (->list of) :length length))))
    (setf (value d)(value (pick of)))
    d))

(defmethod pattern-length ((d dice) &key &allow-other-keys)
  (slot-value d 'length))

(defmethod next-1 ((q dice))
  (setf (pointer q)
	(rem (1+ (pointer q))(pattern-length q)))
  (setf (value q)(next-1 (pick (elements q)))))



