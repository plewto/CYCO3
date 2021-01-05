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

(defmethod clone ((mother recaman-generator) &key new-name new-parent)
  (declare (ignore new-name new-parent))
  (let ((daughter (recaman-generator (slot-value mother 'initial-value)
				     :hook (value-hook mother))))
    (setf (current-value daughter)(current-value mother)
	  (slot-value daughter 'seen)(clone (slot-value mother 'seen))
	  (slot-value daughter 'counter)(slot-value mother 'counter))
    daughter))

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

(setf (documentation 'recaman-generator 'function)
      "Returns generator that produces the Recaman sequence.
https://en.wikipedia.org/wiki/Recam%C3%A1n%27s_sequence
https://oeis.org/A005132

seed  - Integer, initial value. 
:hook - Value hook function, default (lambda (n) n)")
