;;;; cyco-oud chord-variations
;;;;
;;;; Holds all variations of specific chord-type (for a single key).
;;;; IE all c-major chords.
;;;;
;;;; Chord templates are sorted by mean key-number, ignoring muted strings.
;;;;


(defclass chord-variations nil
  ((pitch-class
    :type integer
    :reader pitch-class
    :initarg :pitch-class)
   (chord-type
    :type symbol
    :reader chord-type
    :initform nil
    :initarg :chord-type)
   (variants
    :type vector 			;; of key-numbers.
    :accessor variants
    :initform nil
    :initarg :variations)))

(labels ((sort-variants
	  (vlist)
	  (->vector
	   (sort vlist #'(lambda (a b)
			   (< (mean-keynumber a)
			      (mean-keynumber b)))))))

  (defun make-chord-variations (pitch-class chord-type)
    (make-instance 'chord-variations
		   :pitch-class (pitch-class pitch-class)
		   :chord-type (->cyco-symbol chord-type)
		   :variations #()))

  (defmethod add-variation ((cv chord-variations)(template list))
    (setf (variants cv)
	  (sort-variants (cons (keynumber template)
			       (->list (variants cv)))))) )

(defmethod chord-variant ((cv chord-variations)(index integer))
  (let* ((vary (variants cv))
	 (limit (1- (length vary))))
    (cond ((minusp limit) nil)
	  ((minusp index) nil)
	  ((< index limit)(aref vary index))
	  (t (let ((diff (- index limit))
		   (high (aref vary limit)))
	       (nthcdr diff high))))))

(defmethod dump-chords ((cv chord-variations))
  (let ((pc (keyname (pitch-class cv)))
	(ctype (chord-type cv))
	(vary (variants cv)))
    (dotimes (i (length vary))
      (format t "   pitch ~3A ~8A [variant ~2D] ~A~%"
	      pc ctype i (keyname (aref vary i))))))
  
;;;; TEST
;;;; TEST
;;;; TEST

;;(param baz (make-chord-variations 0 '[test]))
;;(add-variation baz '(60 64 67))
;;(add-variation baz '(64 67 72))
;;(dump-chords baz)

;; (format t "DEBUG ~A~%" (chord-variant baz 0))
;; (format t "DEBUG ~A~%" (chord-variant baz 1))
;; (format t "DEBUG ~A~%" (chord-variant baz 2))
