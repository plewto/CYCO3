;;;; CYCO pattern walker.lisp
;;;;
;;;; The WALKER pattern generates values by random walks. 
;;;;

(in-package :cyco)

(defclass walker (pattern)
  ((current-position
    :type integer
    :accessor walker-current-position
    :initform 0))
  (:documentation
   "Random walks over patterns"))

(defmethod walker-p ((obj walker)) t)

(defun walker (&key of)
  "Creates new instance of WALKER pattern"
  (reset (make-instance 'walker :of (->list of))))

(defmethod value ((w walker))
  (value (nth (walker-current-position w)
	      (elements w))))

(defmethod reset ((w walker))
  (dolist (obj (elements w))(reset obj))
  (setf (walker-current-position w) 0)
  (setf (pointer w) 0)
  w)

(let ((walker-coin (coin :head 1 :tail -1)))
  (defmethod next-1 ((w walker))
    (let ((count (cardinality w))
	  (pos (+ (walker-current-position w)
		  (next-1 walker-coin)))
	  (ptr (pointer w)))
      (setf (pointer w)(rem (1+ ptr) count))
      (setf (walker-current-position w)
	    (cond ((>= pos count) 0)
		  ((minusp pos)(1- count))
		  (t pos)))
      (next-1 (nth (walker-current-position w)
		   (elements w))))))
      
(defmethod clone ((mother walker) &key new-name new-parent)
  (declare (ignore new-name new-parent))
  (walker :of (clone (elements mother))))
