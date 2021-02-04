;;;; CYCO patterns bag.lisp
;;;;
;;;; The BAG pattern generates random values without replacement.
;;;;

(in-package :cyco)

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
  final value indefinitely.  The final 'value' may itself be a pattern."))

(defmethod bag-p ((object bag)) object)

(defun bag (&key of final)
  "Creates new instance of BAG pattern."
  (reset (make-instance 'bag
			:seed (->list of)
			:final final)))

(defmethod reset ((bag bag))
  (call-next-method)
  (setf (elements bag)
	(permute (seed bag)))
  (reset (final-value bag))
  bag)

;; (defmethod next-1 ((bag bag))
;;   (prog1
;;       (or (car (elements bag))
;; 	  (next-1 (final-value bag)))
;;     (setf (elements bag)
;; 	  (cdr (elements bag)))))

(defmethod next-1 ((bag bag))
  (let ((v (or (car (elements bag))
	       (next-1 (final-value bag)))))
    (setf (elements bag)
	  (cdr (elements bag)))
    (setf (value bag) v)))


(defmethod transpose ((bag bag)(x integer))
  (call-next-method)
  (transpose (final-value bag) x)
  bag)

(defmethod invert ((bag bag)(pivot t))
  (call-next-method)
  (invert (final-value bag) pivot)
  bag)

(defmethod retrograde ((bag bag))
  (call-next-method)
  (retrograde (final-value bag))
  bag)

(defmethod clone ((mother bag) &key &allow-other-keys)
  (bag :of (clone (seed mother))
       :final (clone (final-value mother))))
