;;;; CYCO
;;;;
;;;; Terminology:  
;;;;
;;;;   1) A MIDI 'message' is a class or instance or byte array representing a
;;;;      single MIDI command without reference to time.
;;;;
;;;;   2) A MIDI 'event' is a cons of event-time and MIDI message:
;;;;      (time . message)
;;;;
;;;; Hierarchy of MIDI message classes.
;;;;
;;;; MIDI-MESSAGE
;;;;  |
;;;;  +-- MIDI-CHANNEL-MESSAGE
;;;;  |    |
;;;;  |    +-- MIDI-KEY-MESSAGE
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
;;;;  +-- MIDI-SYSTEM-COMMON-MESSAGE
;;;;  |    |
;;;;  |    +--- MIDI-SYSTEM-EXCLUSIVE
;;;;  |    +--- --MIDI-END-SYSTEM-EXCLUSIVE (singleton)
;;;;  |
;;;;  +-- MIDI-META-MESSAGE
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
  "Throws error if value out of range for MIDI data byte (0..127)."
  (if (or (minusp n)(> n 127))
      (progn 
	(cyco-value-error 'assert-midi-data-value n)
	0)
    n))

;;; ---------------------------------------------------------------------- 
;;;			     MIDI-MESSAGE class

(defclass midi-message nil
  ((command 				; MIDI status command
    :type integer			; See constants
    :reader command
    :initarg :command)
   (priority				; Sort order for events 
    :type integer			; with identical times.
    :reader priority		        ; Lower values priorities appear
    :initform 0				; before higher values.
    :initarg :priority)))

(defmethod mnemonic ((evn midi-message))
  (gethash (command evn) +MNEMONICS+))

(defmethod ->string ((evn midi-message))
  (sformat "~A " (mnemonic evn)))

(defmethod midi-message-p ((me midi-message)) t)

(defmethod clone ((evn midi-message) &key new-name new-parent)
  (dismiss new-name new-parent)
  evn)

(defmethod transpose ((e midi-message)(n integer)) e)

(defmethod invert ((e midi-message)(pivot integer)) e)


;;; ---------------------------------------------------------------------- 
;;;			       MIDI-CHANNEL-MESSAGE

(defclass midi-channel-message (midi-message)
  ((channel-index			; MIDI channel index
    :type integer			; 0..15 inclusive.
    :accessor channel-index
    :initform 0
    :initarg :channel-index)
   (data
    :type vector
    :initform #(0 0)
    :accessor data-array
    :initarg :data)))

(defmethod midi-channel-message-p ((obj midi-channel-message)) t)

(defmethod channel ((evn midi-channel-message) &optional _)
  (dismiss _)
  (1+ (channel-index evn)))

(defmethod data ((evn midi-channel-message)(index integer))
  (aref (data-array evn) index))

(defmethod data-count ((evn midi-channel-message)) 2)
  
(defmethod ->string ((evn midi-channel-message))
  (let ((acc (str+ (call-next-method)
		   (sformat "channel: ~2d  " (1+ (channel-index evn))))))
    (dotimes (i (data-count evn))
      (setf acc (str+ acc (sformat "data-~d: ~3d  " (1+ i) (data evn i)))))
    acc))

(defmethod render-midi-message ((evn midi-channel-message))
  (let ((acc (list (+ (command evn)(channel-index evn)))))
    (dotimes (i (data-count evn))
      (push (data evn i) acc))
    (reverse acc)))

;;; ---------------------------------------------------------------------- 
;;;			MIDI-KEY-MESSAGE

(defclass midi-key-message (midi-channel-message)  nil)

(defmethod midi-key-message-p ((evn midi-key-message)) t)

(defmethod keynumber ((mkm midi-key-message))
  (data mkm 0))

(defmethod transpose ((evn midi-key-message)(n integer))
  (let ((kn (+ n (data evn 0))))
    (while (> kn 128)(setf kn (- kn 12)))
    (while (minusp kn)(setf kn (+ kn 12)))
    (setf (aref (data-array evn) 0) kn)
    evn))

(defmethod invert ((evn midi-key-message)(pivot t))
  (if pivot
      (let* ((kn (invert (data evn 0)(keynumber pivot))))
	(while (> kn 128)(setf kn (- kn 12)))
	(while (minusp kn)(setf kn (+ kn 12)))
	(setf (aref (data-array evn) 0) kn)
	evn)
    evn))

(defclass midi-note-off (midi-key-message)
  ((command
    :initform +NOTE-OFF+)
   (priority
    :initform 11)))

(defclass midi-note-on (midi-key-message)
  ((command
    :initform +NOTE-ON+)
   (priority
    :initform 10)))

(defclass midi-poly-pressure (midi-key-message)
  ((command
    :initform +POLY-PRESSURE+)
   (priority
    :initform 9)))

(defmethod midi-note-off-p ((evn midi-note-off)) t)
(defmethod midi-note-on-p ((evn midi-note-on)) t)
(defmethod midi-poly-pressure-p ((evn midi-poly-pressure)) t)

(defun midi-note-off (channel-index keynumber velocity)
  "Creates instance of MIDI-NOTE-OFF.
NOTE: Note off velocity is defined for completeness but is not otherwise supported."
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
  "Creates instance of MIDI-POLY-PRESSURE.
NOTE: Poly-pressure is defined for completeness but is not otherwise supported."
  (make-instance 'midi-poly-pressure
		 :channel-index (assert-midi-channel-index channel-index)
		 :data (vector (assert-midi-data-value keynumber)
			       (assert-midi-data-value pressure))))
	     
;;; ---------------------------------------------------------------------- 
;;;			    MIDI-CONTROL-CHANGE
  
(defclass midi-control-change (midi-channel-message)
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

(defclass midi-channel-pressure (midi-channel-message)
  ((command
    :initform +CHANNEL-PRESSURE+)
   (priority
    :initform 9)))

(defmethod midi-channel-pressure-p ((evn midi-channel-pressure)) t)

(defmethod data-count ((evn midi-channel-pressure)) 1)

(defun midi-channel-pressure (channel-index pressure)
  "Creates instance of MIDI-CHANNEL-PRESSURE."
  (make-instance 'midi-channel-pressure
		 :channel-index (assert-midi-channel-index channel-index)
		 :data (vector (assert-midi-data-value pressure) 0)))

;;; ---------------------------------------------------------------------- 
;;;			    MIDI-PROGRAM-CHANGE

(defclass midi-program-change (midi-channel-message)
  ((command
    :initform +PROGRAM-CHANGE+)
   (priority
    :initform 9)))

(defmethod midi-program-change-p ((evn midi-program-change)) t)

(defmethod data-count ((evn midi-program-change)) 1)

(defun midi-program-change (channel-index program-number)
  "Creates instance of MIDI-PROGRAM-CHANGE."
  (make-instance 'midi-program-change
		 :channel-index (assert-midi-channel-index channel-index)
		 :data (vector (assert-midi-data-value program-number) 0)))


;;; ---------------------------------------------------------------------- 
;;;			      MIDI-PITCH-BEND

(defclass midi-pitch-bend (midi-channel-message)
  ((command
    :initform +PITCH-BEND+)
   (priority
    :initform 9)))

(defmethod midi-pitch-bend-p ((evn midi-pitch-bend)) t)

(defun midi-pitch-bend (channel-index lsb msb)
  "Creates instance of MIDI-PITCH-BEND."
  (make-instance 'midi-pitch-bend
		 :channel-index (assert-midi-channel-index channel-index)
		 :data (vector (assert-midi-data-value lsb)
			       (assert-midi-data-value msb))))



