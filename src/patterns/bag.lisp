;;;; CYCO3 src/patterns/bag
;;;;

(defclass bag (pattern)
  ((seed
    :type list
    :reader seed
    :initform '()
    :initarg :seed)
   (final-value
    :type t
    :reader final-value
    :initform nil
    :initarg :final))
  (:documentation
   "A BAG is a PATTERN which returns elements at random without
  replacement.  Once all elements have been retrieved, bag returns a
  final value indefinitely.  The final value may be a pattern."))

(defmethod bag-p ((obj t)) nil)

(defmethod bag-p ((obj bag)) obj)

(defun bag (&key of final)
  (reset (make-instance 'bag
			     :seed (->list of)
			     :final final)))

(defmethod reset ((b bag))
  (call-next-method)
  (setf (elements b)
	(permute (seed b)))
  (reset (final-value b))
  b)

(defmethod next-1 ((b bag))
  (prog1
      (or (car (elements b))
	  (next-1 (final-value b)))
    (setf (elements b)
	  (cdr (elements b)))))

