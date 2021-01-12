;;;; CYCO util table-wrapper
;;;;
;;;; Defines generic functions for wrapping list and simple-vectors as functions
;;;;

(in-package :cyco)

(defmethod wrap ((table list) &key out-of-bounds-value)
  (let ((limit (length table)))
    #'(lambda (n)
	(cond ((or (minusp n)(>= n limit))
	       out-of-bounds-value)
	      (t (nth n table))))))

(defmethod wrap ((table vector) &key out-of-bounds-value)
  (let ((limit (length table)))
    #'(lambda (n)
	(cond ((or (minusp n)(>= n limit))
	       out-of-bounds-value)
	      (t (aref table n))))))
	
