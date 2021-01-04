;;;; CYCO generators/recaman
;;;;
;;;; Recaman sequence generator.
;;;; https://en.wikipedia.org/wiki/Recam%C3%A1n%27s_sequence
;;;; https://oeis.org/A005132
;;;;


(defclass recaman-generator (generator)
  ((seen
    :type list
    :initform '())
   (initial-value
    :type integer
    :initform 16
    :initarg :seed)
   (counter
    :type integer
    :initform 0)))

(defmethod reset ((rec recaman-generator))
  (setf (current-value rec)
	(slot-value rec 'initial-value)
	(slot-value rec 'seen) '()
	(slot-value rec 'counter) 0)
  rec)

(defun recaman-generator (seed &key (hook #'(lambda (n) n)) &allow-other-keys)
  (reset (make-instance 'recaman-generator :seed seed :hook hook)))

(defmethod next-1 ((rec recaman-generator))
  (prog1
      (value rec)
    (let* ((v0 (current-value rec))
	   (n (1+ (slot-value rec 'counter)))
	   (v1 (- v0 n)))
      (setf v1 (cond ((zerop n) 1)
		     ((and (plusp v1)
			   (not (member v1 (slot-value rec 'seen))))
		      v1)
		      (t (+ v0 n))))
      (push v1 (slot-value rec 'seen))
      (setf (slot-value rec 'counter) n
	    (current-value rec) v1))))

;; TODO implement recaman clone
;; TODO add recaman docstring
