;;;; CYCO patterns/switch
;;;;
;;;; A Switch is a pattern which selects a specific entity from 
;;;; a list of possible entities.  
;;;;

(in-package :cyco)

(defclass switch (pattern)
  ((step-selected-only
    :type t
    :initform nil
    :accessor step-selected-only
    :initarg :step-selected-only)
   (length
    :type integer
    :initform 128
    :initarg :length))
  (:documentation
   "A Switch selects current-value from a list of possible entities.
The entities may be Patterns, Generators, static-values, or any object
for which the next-1 and value methods are defined."))
 
(labels ((reset-all (switch)
		    (dolist (i (elements switch))
		      (reset i)))

	 (step-elements (switch)
			(if (step-selected-only switch)
			    (next-1 (nth (pointer switch)(elements switch)))
			  (dolist (e (elements switch))
			    (next-1 e)))) )

  (defmethod reset ((switch switch))
    (reset-all switch)
    (setf (pointer switch) 0)
    (setf (value switch)(car (elements switch)))
    switch)

  (defun switch (&key of step-selected-only (length 128))
    (reset (make-instance 'switch :of (->list of)
			  :step-selected-only step-selected-only
			  :length length)))

  (defmethod clone ((mother switch) &key &allow-other-keys)
    (switch :of (clone (elements mother))
	    :step-selected-only (step-selected-only mother)
	    :length (pattern-length mother)))
  
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

(setf (documentation 'switch 'function)
      "Creates new switch pattern.

(switch &key of step-selected-only)

:of - List of possible entities.
      The next-1 and value methods must be defined for each element.
      Default '()

:step-selected-only - Boolean.  
      Changes how embedded elements are handled when the next-1 method
      is executed.   If step-selected-only is false, then whenever next-1 
      is called for the switch, next-1 is called for all nested elements.
      If true, next-1 is only called on the currently selected element.
      Default false.")
