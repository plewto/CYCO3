;;;; CYCO pattern merger
;;;;

(defclass merger (pattern)
  ((hook-a
    :type function
    :accessor merger-a-hook
    :initform #'identity
    :initarg :a-hook)
   (hook-b
    :type function
    :accessor merger-b-hook
    :initform #'identity
    :initarg :b-hook)
   (mixer
    :type function
    :accessor merger-mixer-hook
    :initform #'(lambda (a b)(cons a b))
    :initarg :mixer-hook)))

(defun merger (a b &key
		 (a-hook #'identity)
		 (b-hook #'identity)
		 (mixer-hook #'(lambda (a b)(cons a b))))
  (reset (make-instance 'merger :of (list a b)
			:a-hook a-hook
			:b-hook b-hook
			:mixer-hook mixer-hook)))

;; (defmethod value ((m merger))
;;   (let ((a (car (elements m)))
;; 	(b (second (elements m))))
;;     (funcall (merger-mixer-hook m)
;; 	     (funcall (merger-a-hook m)(value a))
;; 	     (funcall (merger-b-hook m)(value b)))))

(defmethod value ((m merger))
  (let ((a (car (elements m)))
	(b (second (elements m))))
    (funcall (merger-mixer-hook m)
	     (funcall (merger-a-hook m)(value a))
	     (funcall (merger-b-hook m)(value b)))))

	     
(defmethod reset ((m merger))
  (reset (car (elements m)))
  (reset (second (elements m)))
  m)

(defmethod cardinality ((m merger))
  (let ((a (cardinality (car (elements m))))
	(b (cardinality (second (elements m)))))
    (if (= a b) a (* a b))))

(defmethod clone ((m merger) &key new-name new-parent)
  (dismiss new-name new-parent)
  (let* ((e (elements m))
	 (a (clone (car e)))
	 (b (clone (second e))))
    (merger a b
	    :a-hook (merger-a-hook m)
	    :b-hook (merger-b-hook m)
	    :mixer-hook (merger-mixer-hook m))))

(defmethod next-1 ((m merger))
  (setf (pointer m)
	(rem (1+ (pointer m))
	     (cardinality m)))
  (next-1 (car (elements m)))
  (next-1 (second (elements m)))
  (value m))

(defmethod retrograde ((m merger))
  (dolist (obj (elements m))
    (retrograde obj))
  m)
