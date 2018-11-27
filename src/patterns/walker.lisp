;;;; CYCO walker pattern
;;;; Random Walk patterns

(defclass walker (pattern) nil
  (:documentation
   "Random walks over patterns"))

(defmethod walker-p ((obj walker)) t)

(defun walker (&key of)
  "Creates new instance of WALKER pattern"
  (reset (make-instance 'walker :of (->list of))))

(let ((walker-coin (coin :head 1 :tail -1)))

  (defmethod next-1 ((w walker))
    (let ((ptr (+ (next-1 walker-coin)
		  (pointer w))))
      (setf ptr (cond ((>= ptr (length (elements w))) 0)
		      ((minusp ptr)(1- (length (elements w))))
		      (t ptr)))
      (prog1
	  (next-1 (nth ptr (elements w)))
	(setf (pointer w) ptr)))))


