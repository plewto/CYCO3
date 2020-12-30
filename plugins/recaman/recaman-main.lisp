;;;; recaman-main
;;;;
;;;; CYCO Pattern to generate Recaman's sequence
;;;;
;;;; https://en.wikipedia.org/wiki/Recam%C3%A1n%27s_sequence
;;;; https://oeis.org/A005132
;;;;

(in-package :cyco)

(defclass recaman (pattern)
  ((seen
    :type list
    :initform '())
   (current-value
    :type integer
    :initform 0
    :initarg :seed)
   (period
    :type integer
    :initform 16
    :initarg :period)
   (seed
    :type integer
    :initform 0
    :initarg :seed)
   (value-hook
    :type function
    :initform #'(lambda (n) n)
    :initarg :hook)))
    
(defgeneric recaman-p (item))
(defmethod recaman-p ((item t)) nil)
(defmethod recaman-p ((item recaman)) t)

(defmethod elements ((rec recaman))
  (slot-value rec 'seen))


(defun recaman (seed &key (hook #'(lambda (n) n))(period 16))
  (make-instance 'recaman
		 :period period
		 :seed seed
		 :hook hook))


(defmethod reset ((rec recaman))
  (setf (slot-value rec 'seen) '()
	(slot-value rec 'current-value)(slot-value rec 'seed)
	(slot-value rec 'pointer) 0)
  rec)


(defmethod clone ((mother recaman) &key new-name new-parent)
  (declare (ignore new-name new-parent))
  (let ((daughter (recaman (slot-value mother 'seed)
			   :hook (slot-value mother 'value-hook)
			   :period (slot-value mother 'period))))
    (setf (slot-value daughter 'current-value)
	  (slot-value mother 'current-value))
    (setf (slot-value daughter 'pointer)
	  (slot-value mother 'pointer))
    daughter))


(defmethod value ((rec recaman))
  (funcall (slot-value rec 'value-hook)
	   (slot-value rec 'current-value)))

(defmethod cardinality ((rec recaman))
  (slot-value rec 'period))

(defmethod next-1 ((rec recaman))
  (let* ((v-out (value rec))
	 (v0 (slot-value rec 'current-value))
	 (n (1+ (slot-value rec 'pointer)))
	 (v1 (- v0 n)))
    (setf v1 (cond ((zerop n)
		    1)
		   ((and (plusp v1)(not (member v1 (slot-value rec 'seen))))
		    v1)
		   (t (+ v0 n))))
    (push v1 (slot-value rec 'seen))
    (setf (slot-value rec 'pointer) n
	  (slot-value rec 'current-value) v1)
    v-out))


(flet ((next-all (rec)
		 (reset rec)
		 (next rec (cardinality rec))))

  
  (defmethod next ((rec recaman) &optional n)
    (cond ((eq n :all)
	   (next-all rec))
	  (t (call-next-method)))))
	   
    
    
    
