;;;; CYCO generators/generator.lisp
;;;;


(in-package :cyco)

(defclass generator nil
  ((value-hook
    :type function
    :initform #'(lambda (n) n)
    :accessor value-hook
    :initarg :hook)
   (current-value
    :type number
    :initform 0
    :accessor current-value
    :initarg :seed)))
  

(defmethod value ((gen generator))
  (funcall (slot-value gen 'value-hook)
	   (slot-value gen 'current-value)))

(defmethod reset ((gen generator)) gen)

(defmethod next-1 ((gen generator)) (value gen))


(defmethod next-n ((gen generator)(n integer))
  (loop for i from 1 to n
	collect (next-1 gen)))

(defmethod next ((gen generator) &optional (n 1))
  (cond ((= n 1)(next-1 gen))
	((integerp n)(next-n gen n))
	(t (value gen))))



(defclass constant-value (generator) nil)

(defun constant-value (n &key &allow-other-keys)
  (make-instance 'constant-value :seed n))

(defmethod clone ((mother constant-value) &key new-name new-parent)
  (declare (ignore new-name new-parent))
  (constant-value (value mother))) 



(defclass counter (generator) nil)

(defmethod reset ((gen counter))
  (setf (current-value gen) 0)
  gen)

(defun counter (&key (hook #'(lambda (n) n)) &allow-other-keys)
  (reset (make-instance 'counter :hook hook)))

(defmethod clone ((mother counter) &key new-name new-parent)
  (declare (ignore new-name new-parent))
  (let ((daughter (counter :hook (value-hook mother))))
    (setf (current-value daughter)(current-value mother))
    daughter))

(defmethod next-1 ((gen counter))
  (prog1
      (value gen)
    (setf (current-value gen)(1+ (current-value gen)))))


(defclass down-counter (generator)
  ((initial-value
    :type integer
    :accessor initial-value
    :initform 16
    :initarg :seed)))

(defmethod reset ((gen down-counter))
  (setf (current-value gen)
	(initial-value gen))
  gen)

(defun down-counter (n &key (hook #'(lambda (n) n)) &allow-other-keys)
  (reset (make-instance 'down-counter :seed n :hook hook)))

(defmethod clone ((mother down-counter) &key new-name new-parent)
  (declare (ignore new-name new-parent))
  (let ((daughter (down-counter (initial-value mother)
				:hook (value-hook mother))))
    (setf (current-value daughter)
	  (current-value mother))
    daughter))


(defmethod next-1 ((gen down-counter))
  (prog1
      (value gen)
    (setf (current-value gen)
	  (max (1- (current-value gen)) 0))))
