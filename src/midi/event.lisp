;;;; PigIron cyco midi event
;;;;
;;;; Defines hierarchy of MIDI event classes.  All instances of MIDI events
;;;; are immutable.
;;;;
;;;; MIDI-EVENT
;;;;  |
;;;;  +-- MIDI-CHANNEL-EVENT
;;;;  |    |
;;;;  |    +-- MIDI-KEY-EVENT
;;;;  |    |    |
;;;;  |    |    +-- MIDI-NOTE-OFF
;;;;  |    |    +-- MIDI-NOTE-ON
;;;;  |    |    +-- MIDI-POLY-PRESSURE
;;;;  |    |
;;;;  |    +-- MIDI-CONTROL-CHANGE
;;;;  |    +-- MIDI-CHANNEL-PRESSURE
;;;;  |    +-- MIDI-PROGRAM-CHANGE
;;;;  |    +-- MIDI-PITCH-BEND
;;;;  |
;;;;  +-- MIDI-SYSTEM-COMMON-EVENT
;;;;  |    |
;;;;  |    +--- MIDI-SYSTEM-EXCLUSIVE
;;;;  |    +--- --MIDI-END-SYSTEM-EXCLUSIVE (singleton)
;;;;  |
;;;;  +-- MIDI-META-EVENT
;;;;       |
;;;;       +-- MIDI-META-TEXT
;;;;       |    |
;;;;       |    +-- MIDI-META-COPYRIGHT
;;;;       |    +-- MIDI-META-TRACK-NAME
;;;;       |    +-- MIDI-META-INSTRUMENT-NAME
;;;;       |    +-- MIDI-META-LYRIC
;;;;       |    +-- MIDI-META-CUE
;;;;       |    +-- MIDI-META-MARKER
;;;;       |
;;;;       +-- --MIDI-END-OF-TRACK (singleton)
;;;;       +-- MIDI-TEMPO-CHANGE
;;;;       +-- MIDI-TIME-SIGNATURE
;;;;       +-- MIDI-KEY-SIGNATURE


(defun assert-midi-channel-index (ci)
  "Throws error if value not valid MIDI channel index (0..15)."
  (if (or (minusp ci)(> ci 15))
      (progn 
	(cyco-value-error 'assert-midi-channel-index ci)
	0)
    ci))

(defun assert-midi-data-value (n)
  "Throws error if value out of range for MIDI datya byte (0..127)."
  (if (or (minusp n)(> n 127))
      (progn 
	(cyco-value-error 'assert-midi-data-value n)
	0)
    n))

;;; ---------------------------------------------------------------------- 
;;;			     MIDI-EVENT class

(defclass midi-event nil
  ((command 				; MIDI status command
    :type integer			; See constants
    :reader command
    :initarg :command)
   (priority				; Sort order for events 
    :type integer			; with identical times.
    :reader priority		        ; Lower values priorities appear
    :initform 0				; before higher values.
    :initarg :priority)))

  (defmethod mnemonic ((evn midi-event))
    (gethash (command evn) +MNEMONICS+))

  (defmethod ->string ((evn midi-event))
    (sformat "~A " (mnemonic evn)))
  
(defmethod midi-event-p ((obj t)) nil)
(defmethod midi-event-p ((me midi-event)) t)

(defmethod clone ((evn midi-event) &key new-name new-parent)
  (dismiss new-name new-parent)
  evn)

(defmethod transpose ((e midi-event)(n integer)) e)

(defmethod invert ((e midi-event)(pivot integer)) e)


;;; ---------------------------------------------------------------------- 
;;;			       MIDI-CHANNEL-EVENT

(defclass midi-channel-event (midi-event)
  ((channel-index			; MIDI channel index
    :type integer			; between 0..15 inclusive.
    :accessor channel-index
    :initform 0
    :initarg :channel-index)
   (data				; MIDI data
    :type vector
    :initform #(0 0)
    :accessor data-array
    :initarg :data)))

(defmethod midi-channel-event-p ((obj t)) nil)
(defmethod midi-channel-event-p ((obj midi-channel-event)) t)

(defmethod channel ((evn midi-channel-event) &optional _)
  (dismiss _)
  (1+ (channel-index evn)))

(defmethod data ((evn midi-channel-event)(index integer))
  (aref (data-array evn) index))

(defmethod data-count ((evn midi-channel-event)) 2)
  
(defmethod ->string ((evn midi-channel-event))
  (let ((acc (str+ (call-next-method)
		   (sformat "channel: ~2d  " (1+ (channel-index evn))))))
    (dotimes (i (data-count evn))
      (setf acc (str+ acc (sformat "data-~d: ~3d  " (1+ i) (data evn i)))))
    acc))

(defmethod render-midi-event ((evn midi-channel-event))
  (let ((acc (list (+ (command evn)(channel-index evn)))))
    (dotimes (i (data-count evn))
      (push (data evn i) acc))
    (reverse acc)))

;;; ---------------------------------------------------------------------- 
;;;			MIDI-KEY-EVENT

(defclass midi-key-event (midi-channel-event)  nil)

(defmethod midi-key-event-p ((obj t)) nil)
(defmethod midi-key-event-p ((evn midi-key-event)) t)

(defmethod transpose ((evn midi-key-event)(n integer))
  (let ((kn (+ n (data evn 0))))
    (while (> kn 128)(setf kn (- kn 12)))
    (while (minusp kn)(setf kn (+ kn 12)))
    (setf (aref (data-array evn) 0) kn)
    evn))

(defmethod invert ((evn midi-key-event)(pivot t))
  (let* ((kn (invert (data evn 0)(keynumber pivot))))
    (while (> kn 128)(setf kn (- kn 12)))
    (while (minusp kn)(setf kn (+ kn 12)))
    (setf (aref (data-array evn) 0) kn)
    evn))

(defclass midi-note-off (midi-key-event)
  ((command
    :initform +NOTE-OFF+)
   (priority
    :initform 11)))

(defclass midi-note-on (midi-key-event)
  ((command
    :initform +NOTE-ON+)
   (priority
    :initform 10)))

(defclass midi-poly-pressure (midi-key-event)
  ((command
    :initform +POLY-PRESSURE+)
   (priority
    :initform 9)))

(defmethod midi-note-off-p ((obj t)) nil)
(defmethod midi-note-off-p ((evn midi-note-off)) t)
(defmethod midi-note-on-p ((obj t)) nil)
(defmethod midi-note-on-p ((evn midi-note-on)) t)
(defmethod midi-poly-pressure-p ((obj t)) nil)
(defmethod midi-po(ly-pressure-p (evn midi-poly-pressure)) t)

(defun midi-note-off (channel-index keynumber velocity)
  "Creates instance of MIDI-NOTE-OFF."
  (make-instance 'midi-note-off
		 :channel-index (assert-midi-channel-index channel-index)
		 :data (vector (assert-midi-data-value keynumber)
			       (assert-midi-data-value velocity))))

(defun midi-note-on (channel-index keynumber velocity)
  "Creates instance of MIDI-NOTE-ON.
If velocity is 0, returns MIDI-NOTE-OFF instead."
  (if (zerop velocity)
      (midi-note-off channel-index keynumber 0)
    (make-instance 'midi-note-on
		   :channel-index (assert-midi-channel-index channel-index)
		   :data (vector (assert-midi-data-value keynumber)
				 (assert-midi-data-value velocity)))))

(defun midi-poly-pressure (channel-index keynumber pressure)
  "Creates instance of MIDI-POLY-PRESSURE."
  (make-instance 'midi-poly-pressure
		 :channel-index (assert-midi-channel-index channel-index)
		 :data (vector (assert-midi-data-value keynumber)
			       (assert-midi-data-value pressure))))
	     
;;; ---------------------------------------------------------------------- 
;;;			    MIDI-CONTROL-CHANGE
  
(defclass midi-control-change (midi-channel-event)
  ((command
    :initform +CONTROL-CHANGE+)
   (priority
    :initform 9)))

(defmethod midi-control-change-p ((obj t)) nil)
(defmethod midi-control-change-p ((evn midi-control-change)) t)

(defun midi-control-change (channel-index controller-number value)
  "Creates instance of MIDI-CONTROL-CHANGE."
  (make-instance 'midi-control-change
		 :channel-index (assert-midi-channel-index channel-index)
		 :data (vector (assert-midi-data-value controller-number)
			       (assert-midi-data-value value))))

;;; ---------------------------------------------------------------------- 
;;;			   MIDI-CHANNEL-PRESSURE

(defclass midi-channel-pressure (midi-channel-event)
  ((command
    :initform +CHANNEL-PRESSURE+)
   (priority
    :initform 9)))

(defmethod midi-channel-pressure-p ((obj t)) nil)
(defmethod midi-channel-pressure-p ((evn midi-channel-pressure)) t)

(defmethod data-count ((evn midi-channel-pressure)) 1)

(defun midi-channel-pressure (channel-index pressure)
  "Creates instance of MIDI-CHANNEL-PRESSURE."
  (make-instance 'midi-channel-pressure
		 :channel-index (assert-midi-channel-index channel-index)
		 :data (vector (assert-midi-data-value pressure) 0)))

;;; ---------------------------------------------------------------------- 
;;;			    MIDI-PROGRAM-CHANGE

(defclass midi-program-change (midi-channel-event)
  ((command
    :initform +PROGRAM-CHANGE+)
   (priority
    :initform 9)))

(defmethod midi-program-change-p ((obj t)) nil)
(defmethod midi-program-change-p ((evn midi-program-change)) t)

(defmethod data-count ((evn midi-program-change)) 1)

(defun midi-program-change (channel-index program-number)
  "Creates instance of MIDI-PROGRAM-CHANGE."
  (make-instance 'midi-program-change
		 :channel-index (assert-midi-channel-index channel-index)
		 :data (vector (assert-midi-data-value program-number) 0)))


;;; ---------------------------------------------------------------------- 
;;;			      MIDI-PITCH-BEND

(defclass midi-pitch-bend (midi-channel-event)
  ((command
    :initform +PITCH-BEND+)
   (priority
    :initform 9)))

(defmethod midi-pitch-bend-p ((obj t)) nil)
(defmethod midi-pitch-bend-p ((evn midi-pitch-bend)) t)

(defun midi-pitch-bend (channel-index lsb msb)
  "Creates instance of MIDI-PITCH-BEND."
  (make-instance 'midi-pitch-bend
		 :channel-index (assert-midi-channel-index channel-index)
		 :data (vector (assert-midi-data-value lsb)
			       (assert-midi-data-value msb))))
