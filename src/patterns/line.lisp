;;;; CYCO
;;;;

(defclass line (pattern) nil
  (:documentation
   "A LINE is a PATTERN which continues to return it's final value
once all previous values have been returned."))

(defmethod line-p ((obj line)) obj)

(defun line (&key (of '()))
  (let ((q (make-instance 'line :of (->list of))))
    (reset q)
    q))

(defmethod next-1 ((q line))
  (let* ((ptr (pointer q))
	 (val (next-1 (nth ptr (elements q)))))
    (setf (pointer q)
	  (min (1+ ptr)(1- (cardinality q))))
    (setf (slot-value q 'value) val)
    val))




