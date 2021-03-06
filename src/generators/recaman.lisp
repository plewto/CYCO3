;;;; CYCO generators/recaman
;;;;
;;;; Recaman sequence generator.
;;;; https://en.wikipedia.org/wiki/Recam%C3%A1n%27s_sequence
;;;; https://oeis.org/A005132
;;;;

(in-package :cyco)

(defclass recaman (generator)
  ((seen
    :type list
    :initform '())
   (initial-value
    :type integer
    :initform 16
    :initarg :seed)
   (counter
    :type integer
    :initform 0)
   (length
    :type integer
    :initform 128
    :initarg :length))
  (:documentation
   "RECAMAN is a generator for producing a Recaman sequence
https://en.wikipedia.org/wiki/Recam%C3%A1n%27s_sequence
https://oeis.org/A005132"))

(defmethod reset ((rec recaman))
  (setf (internal-value rec)
	(slot-value rec 'initial-value)
	(slot-value rec 'seen) '()
	(slot-value rec 'counter) 0)
  rec)

(defun recaman (seed &key
		     (hook #'(lambda (n) n))
		     (length 128)
		     &allow-other-keys)
  (reset (make-instance 'recaman
			:seed seed 
			:hook hook 
			:length length)))

(defmethod clone ((mother recaman) &key &allow-other-keys)
  (recaman (slot-value mother 'initial-value)
	   :hook (value-hook mother)
	   :length (pattern-length mother)))

(defmethod next-1 ((rec recaman))
  (prog1
      (value rec)
    (let* ((v0 (internal-value rec))
	   (n (1+ (slot-value rec 'counter)))
	   (v1 (- v0 n)))
      (setf v1 (cond ((zerop n) 1)
		     ((and (plusp v1)
			   (not (member v1 (slot-value rec 'seen))))
		      v1)
		     (t (+ v0 n))))
      (push v1 (slot-value rec 'seen))
      (setf (slot-value rec 'counter) n
	    (internal-value rec) v1))))

(defmethod pattern-length ((r recaman) &key &allow-other-keys)
  (slot-value r 'length))


(setf (documentation 'recaman 'function)
      "Returns new RECAMAN generator.

(RECAMAN seed &key hook)

seed - Integer, intial value
:hook    - Function applied by value method to the internal value.
           Default (lambda (n) n)")

