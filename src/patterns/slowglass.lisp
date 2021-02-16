;;;; CYCO patterns/slowglass.lisp
;;;;
;;;; SLOWGLASS pattern reduces the sample-rate of an embedded pattern
;;;; or generator.

(in-package :cyco)

(defclass slowglass (pattern)
  ((rate
    :type integer
    :accessor slowglass-rate
    :initform 0
    :initarg :divide))
  (:documentation
   "Samples source pattern on every nth call to next-1."))

(defmethod reset ((glass slowglass))
  (call-next-method)
  (let ((n (slowglass-rate glass)))
    (setf (pointer glass)
	  (cond ((<= 1 n) 0)
		((= 2 n) 1)
		(t (- n 2)))))
  glass)

(defun slowglass (source &key (n 1))
  "(SLOWGLASS source &key n)

Samples source pattern on every nth call to next-1

source - Pattern or Generator or any object where next-1 is defined.
:n     - Sample rate, default 1."
  (reset (make-instance 'slowglass
			:of (->list source)
			:divide (max 0 n))))

(defmethod clone ((mother slowglass) &key &allow-other-keys)
  (slowglass (clone (elements mother)) :n (slowglass-rate mother)))

(defmethod next-1 ((glass slowglass))
  (if (zerop (pointer glass))
      (let ((pattern (car (elements glass))))
	(setf (pointer glass)
	      (max 0 (1- (slowglass-rate glass))))
	(setf (value glass)
	      (next-1 pattern)))
    (progn
      (setf (pointer glass)(1- (pointer glass)))
      (value glass))))

(defmethod pattern-length ((glass slowglass) &key (max 128) &allow-other-keys)
  (* (pattern-length (car (elements glass)) :max max)
     (max 1 (slowglass-rate glass))))
