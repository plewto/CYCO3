;;;; CYCO pattern line.lisp
;;;;
;;;; A LINE pattern returns items in sequence, then stops.
;;;;

(in-package :cyco)

(defclass line (pattern) nil
  (:documentation
   "A LINE is a PATTERN which continues to return its final value
once all previous values have been returned."))

(defmethod line-p ((object line)) object)

(defun line (&key (of '()))
  "Creates new instance of LINE pattern."
  (let ((q (make-instance 'line :of (->list of))))
    (reset q)
    q))

(defmethod next-1 ((q line))
  (let* ((ptr (pointer q))
	 (val (next-1 (nth ptr (elements q)))))
    (setf (pointer q)
	  (min (1+ ptr)(1- (pattern-length q))))
    (setf (slot-value q 'value) val)
    val))

