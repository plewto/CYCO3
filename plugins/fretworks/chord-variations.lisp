;;;; CYCO fretworks plugin
;;;;
;;;; Holds all variations of specific chord type for a single key,
;;;; i.e. all c major chord forms.
;;;;
;;;; Chord templates are sorted by mean key-number, ignoring rest.
;;;; Variation retrieval indexed by octave.
;;;; If octave is below min-octave return nil
;;;; If octave is above variation count (considering min-octave offset), return 
;;;; 'slice' of highest variation.  As the octave number increases more 
;;;; successively more of the Left handed notes are sliced from the chord.

(defclass chord-variations nil
  ((pitch-class
    :type integer
    :reader pitch-class
    :initarg :pitch-class)
   (variants
    :type vector
    :accessor variants
    :initform nil
    :initarg :var)
   (minimum-octave
    :type integer
    :accessor minimum-octave
    :initform 0
    :initarg :minimum-octave)))

(labels ((sort-variants (vlist)
		     (->vector
		      (sort vlist #'(lambda (a b)
				      (< (mean-keynumber a)
					 (mean-keynumber b)))))) )

  (defun make-chord-variations (pitch-class minimum-octave)
    (make-instance 'chord-variations
		   :pitch-class (pitch-class pitch-class)
		   :minimum-octave minimum-octave
		   :var #()))

  (defmethod add-variation ((cv chord-variations)(template list))
    (setf (variants cv)
  	  (sort-variants (cons (keynumber template)
  			       (->list (variants cv)))))) )

(defmethod chord-variant ((cv chord-variations)(n integer))
  (let* ((vary (variants cv))
	 (limit (1- (length vary)))
	 (octave-offset (minimum-octave cv)))
    (setf n (- n octave-offset))
    (cond ((minusp n) nil)
	  ((< n limit)(aref vary n))
	  (t (let ((diff (- n limit))
		   (high (aref vary limit)))
	       (nthcdr diff high))))))
	       
(defmethod dump-chords ((cv chord-variations))
  (let ((pc (keyname (pitch-class cv)))
	(offset (minimum-octave cv))
	(vec (variants cv)))
    (dotimes (i (length vec))
      (format t "    [pitch ~2D octave ~2D] ~A~%" pc (+ i offset)(keyname (aref vec i))))))
    
