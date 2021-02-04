;;;; CYCO patterns/switch
;;;;
;;;; A Switch is a pattern which selectes value from any number
;;;; of patterns.
;;;;

(in-package :cyco)

(defclass switch (pattern)
  ((step-only-selected
    :type t
    :initform nil
    :accessor step-only-selected
    :initarg :step-only-selected)))
 
(labels ((reset-all (switch)
		    (dolist (i (elements switch))
		      (reset i)))

	 (step-elements (switch)
			(if (step-only-selected switch)
			    (next-1 (nth (pointer switch)(elements switch)))
			  (dolist (e (elements switch))
			    (next-1 e)))) )

  (defmethod reset ((switch switch))
    (reset-all switch)
    (setf (pointer switch) 0)
    (setf (value switch)(car (elements switch)))
    switch)

  (defun switch (&key of step-only-selected)
    (reset (make-instance 'switch :of (->list of)
			  :step-only-selected step-only-selected)))

  (defmethod clone ((mother switch) &key &allow-other-keys)
    (switch :of (clone (elements mother))
	    :step-only-selected (step-only-selected mother)))

  
  (defmethod select ((switch switch)(item t))
    (let ((len (length (elements switch))))
      (setf (pointer switch)
	    (cond ((integerp item) item)
		  ((eq item :next)
		   (1+ (pointer switch)))
		  ((eq item :previous)
		   (1- (pointer switch)))
		  ((eq item :last)
		   (1- len))
		  ((eq item :random)
		   (random len))
		  (t (pointer switch))))
      (setf (pointer switch)
	    (if (minusp (pointer switch))
		(1- len)
	      (rem (pointer switch) len))))
    (pointer switch))

  (defmethod next-1 ((switch switch))
    (step-elements switch)
    (value (nth (pointer switch)(elements switch))))) 
 
