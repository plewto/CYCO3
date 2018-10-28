;;;; CYCO3 src/patterns/cycle
;;;;

(defclass cycle (pattern) nil
  (:documentation
   "A CYCLE is a PATTERN where elements are drawn in a 
cyclical manner.  Once all elements have been extracted the
cycle repeats."))

(defmethod cycle-p ((obj cycle)) obj)

(defun cycle (&key (of '()))
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

(defmethod ->cycle ((obj cycle)) obj)

(defmethod ->cycle ((lst list))
  (cycle :of lst))

(defmethod ->cycle ((v vector))
  (cycle :of (->list v)))

(defmethod ->cycle ((obj t))
  (cycle :of (->list obj)))

