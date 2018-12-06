;;;; CYCO pattern merger
;;;;

(defclass merger (pattern)
  ((source-a
    :type pattern
    :accessor merger-source-a
    :initform (line :of '(1))
    :initarg :a)
   (hook-a
    :type function
    :accessor merger-hook-a
    :initform #'identity
    :initarg :a-hook)
   (source-b
    :type pattern
    :accessor merger-source-b
    :initform (line :of '(1))
    :initarg :b)
   (hook-b
    :type function
    :accessor merger-hook-b
    :initform #'identity
    :initarg :b-hook)
   (mixer
    :type function
    :accessor merger-mixer
    :initform #'(lambda (a b)(dismiss a b) a)
    :initarg :mixer)))


(defmethod elements ((m merger)) nil)

(defmethod cardinality ((m merger))
  (let ((ca (cardinality (merger-source-a m)))
	(cb (cardinality (merger-source-b m))))
    (if (= ca cb) ca (* ca cb))))

(defmethod reset ((m merger))
  (setf (pointer m) 0)
  (reset (merger-source-a m))
  (reset (merger-source-b m))
  m)

(defun merger (a b &key
		 (a-hook #'identity)
		 (b-hook #'identity)
		 (mixer #'(lambda (a b)(dismiss a b) a)))
  (make-instance 'merger
		 :a a :a-hook a-hook
		 :b b :b-hook b-hook
		 :mixer mixer))

(defmethod clone ((m merger) &key new-name new-parent)
  (dismiss new-name new-parent)
  (merger (clone (merger-source-a m))
	  (clone (merger-source-b m))
	  :a-hook (merger-hook-a m)
	  :b-hook (merger-hook-b m)
	  :mixer (merger-mixer m)))

(defmethod value ((m merger))
  (let ((a-hook (merger-hook-a m))
	(b-hook (merger-hook-b m))
	(mixer (merger-mixer m)))
    (funcall mixer
	     (funcall a-hook (value (merger-source-a m)))
	     (funcall b-hook (value (merger-source-b m))))))

(defmethod next-1 ((m merger))
  (setf (pointer m)
	(rem (1+ (pointer m))(cardinality m)))
  (next-1 (merger-source-a m))
  (next-1 (merger-source-b m))
  (value m))

(defmethod transpose ((m merger))
  (transpose (

;; ----------------- TEST
;;

(param a (cycle :of '(0 1 2)))
(param b (cycle :of '(3 4 5 6)))
(param foo (merger a b :mixer #'(lambda (q r)(+ q r))))


