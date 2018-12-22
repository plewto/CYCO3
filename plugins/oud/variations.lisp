;;;; cyco-oud chord-variations
;;;;
;;;; Holds all variations of specific chord-type for a single key,
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
    "Creates new instance of CHORD-VARIATIONS
pitch-class - an integer or key-number, 0..11
chord-type  - symbol"
    (make-instance 'chord-variations
		   :pitch-class (pitch-class pitch-class)
		   :chord-type (->cyco-symbol chord-type)
		   :variations #()))

  (defmethod add-variation ((cv chord-variations)(template list))
    (setf (variants cv)
	  (sort-variants (cons (keynumber template)
			       (->list (variants cv)))))) )

(defmethod chord-variant ((cv chord-variations)(index integer))
  "Returns chord variation
index - Integer.
        For index < 0, return nil
        For index > variation count, return 'slice' of the 
        highest chord template.  As index increases above 
        variation count, slice progressively more notes from 
        the template.  If index is sufficiently high, return nil."

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
