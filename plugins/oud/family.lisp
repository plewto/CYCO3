;;;; cyco-oud chord-family
;;;;
;;;; A chord-family holds all variations for all keys of a specific chord-type.
;;;; IE all major chords.
;;;;

(in-package :cyco-oud)

(defclass chord-family nil
  ((chord-type
    :type symbol
    :reader chord-type
    :initform nil
    :initarg :chord-type)
   (pitch-classes
    :type t ;; vector
    :accessor pitch-classes
    :initform nil
    :initarg :pitch-classes)))

(defun make-chord-family (chord-type)
  (let ((vary (->vector (copies 12))))
    (dotimes (pc 12)
      (setf (aref vary pc)(make-chord-variations pc chord-type)))
    (make-instance 'chord-family
		   :chord-type (->cyco-symbol chord-type)
		   :pitch-classes vary)))

(defmethod add-chord-variation ((family chord-family)(pitch-class t)(template list))
  (let* ((pc (pitch-class pitch-class))
	 (cvar (aref (pitch-classes family) pc)))
    (add-variation cvar template)))

(defmethod chord-variant ((family chord-family)(key t))
  "Returns key-number list for indicated key.
The octave-value of key selects specific variation.
See method for CHORD-VARIATIONS."
  (let ((pcarray (pitch-classes family)))
    (cond ((not key)(aref pcarray 0))
	  ((rest-p key) nil)
	  (t (let* ((kn (keynumber key))
		    (pc (pitch-class kn))
		    (oct (octave kn))
		    (cv (aref pcarray pc)))
	       (chord-variant cv oct))))))
	       
(defmethod dump-chords ((family chord-family))
  (format t "OUD:CHORD-FAMILY ~A~%" (chord-type family))
  (let ((pcarray (pitch-classes family)))
    (dotimes (i 12)
      (dump-chords (aref pcarray i)))))

(defmethod remove-duplicate-chords ((family chord-family))
  (let ((vary (pitch-classes family)))
    (dotimes (i (length vary))
      (remove-duplicate-chords (aref vary i)))))
