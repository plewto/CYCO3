;;;; CYCO table-wrapper
;;;;
;;;; Defines generic functions for wrapping list and vectors as functions


(defmethod wrap ((table list) &key out-of-bounds-value)
  (let ((limit (length table)))
    #'(lambda (n)
	(cond ((or (minusp n)(>= n limit))
	       out-of-bounds-value)
	      (t (nth n table))))))

(defmethod wrap ((table simple-vector) &key out-of-bounds-value)
  (let ((limit (length table)))
    #'(lambda (n)
	(cond ((or (minusp n)(>= n limit))
	       out-of-bounds-value)
	      (t (aref table n))))))
	
