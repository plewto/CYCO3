;;;; CYCO Cycle Pattern
;;;;

(defclass cycle (pattern) nil
  (:documentation
   "A CYCLE is a PATTERN where elements are drawn in a 
cyclical manner.  Once all elements have been extracted the
cycle repeats."))

(defmethod cycle-p ((object cycle)) object)

(defun cycle (&key (of '()))
  "Cre4ates new instance of cycle pattern."
  (let ((q (make-instance 'cycle :of (->list of))))
    (reset q)
    q))

(defmethod next-1 ((q cycle))
  (let* ((ptr (pointer q))
	 (val (next-1 (nth ptr (elements q)))))
    (setf (pointer q)
	  (rem (1+ ptr)(cardinality q)))
    (setf (slot-value q 'value) val)
    val))

(defmethod ->cycle ((object cycle)) object)

(defmethod ->cycle ((lst list))
  (cycle :of lst))

(defmethod ->cycle ((v vector))
  (cycle :of (->list v)))

(defmethod ->cycle ((object t))
  (cycle :of (->list object)))



