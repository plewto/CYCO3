;;;; CYCO fretworks chord-family
;;;;
;;;; A 'chord-family' holds all variations for all keys of a specific chord type.
;;;; I.E. all major chords.
;;;;

(defclass chord-family nil
  ((chord-type
    :type symbol
    :accessor chord-type
    :initform nil
    :initarg :chord-type)
   (pitch-classes
    :type vector
    :accessor pitch-classes
    :initform nil
    :initarg :pitch-classes)
   (minimum-octave
    :type integer
    :accessor minimum-octave
    :initarg :minimum-octave)))

(defun make-chord-family (chord-type &optional (minimum-octave 0))
  (let ((vary (->vector (copies 12))))
    (dotimes (pc 12)
      (setf (aref vary pc)(make-chord-variations pc minimum-octave)))
    (make-instance 'chord-family
		   :chord-type (->cyco-symbol chord-type)
		   :pitch-classes vary
		   :minimum-octave minimum-octave)))

(defmethod add-chord-variation ((family chord-family)
				       (pitch-class t)
				       (template list))
  (let* ((pc (pitch-class pitch-class))
	 (cvar (aref (pitch-classes family) pc)))
    (add-variation cvar template)))


(defmethod chord-variant ((family chord-family)(key t))
  (let ((pcarray (pitch-classes family)))
    (cond ((not key)
	   (aref pcarray 0))
	  ((rest-p key)
	   nil)
	  (t (let* ((kn (keynumber key))
		  (pc (pitch-class kn))
		  (oct (octave kn))
		  (cv (aref (pitch-classes family) pc)))
	     (chord-variant cv oct))))))

(defmethod dump-chords ((family chord-family))
  (format t "FRETWORKS:CHORD-FAMILY ~A~%" (chord-type family))
  (let ((vec (pitch-classes family)))
    (dotimes (i 12)
      (format t "  ~A~%" (keyname i))
      (dump-chords (aref vec i)))))
