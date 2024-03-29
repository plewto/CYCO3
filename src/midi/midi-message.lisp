;;;; CYCO midi midi-message.lisp
;;;;
;;;; Defines low-level MIDI messages.
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
;;;;  |    +-- MIDI-SYSTEM-EXCLUSIVE
;;;;  |    +-- MIDI-END-SYSTEM-EXCLUSIVE (singleton instance of MIDI-SYSTEM-COMMON-MESSAGE)
;;;;  |    +-- MIDI-RESET (singleton instance of MIDI-SYSTEM-COMMON-MESSAGE)
;;;;  |    +-- MIDI-START (Singleton instance of MIDI-SYSTEM-COMMON-MESSAGE)
;;;;  |    +-- MIDI-STOP (Singleton instance of MIDI-SYSTEM-COMMON-MESSAGE)
;;;;  |    +-- MIDI-CONTINUE (Singleton instance of MIDI-SYSTEM-COMMON-MESSAGE)
;;;;  |    +-- MIDI-CLOCK (Singleton instance of MIDI-SYSTEM-COMMON-MESSAGE)
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
;;;;       +-- --MIDI-END-OF-TRACK ** TODO re-implement singelto (singleton instance of MIDI-SYS)
;;;;       +-- MIDI-TEMPO-CHANGE
;;;;       +-- MIDI-TIME-SIGNATURE
;;;;       +-- MIDI-KEY-SIGNATURE
;;;;

(in-package :cyco)

(defun assert-midi-channel-index (channel-index)
  "Throws error if value not valid MIDI channel index (0..15)."
  (if (or (minusp channel-index)(> channel-index 15))
      (progn 
	(cyco-value-error 'assert-midi-channel-index channel-index)
	0)
    channel-index))

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

(defmethod mnemonic ((message midi-message))
  (gethash (command message) +MNEMONICS+))

(defmethod ->string ((message midi-message))
  (sformat "~A " (mnemonic message)))

(defmethod midi-message-p ((message midi-message)) t)

(defmethod clone ((mother midi-message) &key &allow-other-keys)
  mother)

(defmethod transpose ((message midi-message)(n integer)) message)

(defmethod invert ((message midi-message)(pivot integer)) message)

(defmethod data-count ((message midi-message)) -1)

(defmethod data ((message midi-message)(index integer)) -1)


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

(defmethod midi-channel-message-p ((object midi-channel-message)) t)

(defmethod channel ((message midi-channel-message) &optional _)
  (declare (ignore _))
  (1+ (channel-index message)))

(defmethod data ((message midi-channel-message)(index integer))
  (aref (data-array message) index))

(defmethod data-count ((message midi-channel-message)) 2)
  
(defmethod ->string ((message midi-channel-message))
  (let ((result (str+ (call-next-method)
		      (sformat "channel: ~2d  " (1+ (channel-index message))))))
    (dotimes (i (data-count message))
      (setf result (str+ result (sformat "data-~d: ~3d  " (1+ i) (data message i)))))
    result))

(defmethod render-midi-message ((message midi-channel-message))
  (let ((bytes (list (+ (command message)(channel-index message)))))
    (dotimes (i (data-count message))
      (push (data message i) bytes))
    (reverse bytes)))

;;; ---------------------------------------------------------------------- 
;;;			MIDI-KEY-MESSAGE

(defclass midi-key-message (midi-channel-message)  nil)

(defmethod midi-key-message-p ((message midi-key-message)) t)

(defmethod keynumber ((key-message midi-key-message))
  (data key-message 0))

(defmethod transpose ((message midi-key-message)(n integer))
  (let ((key-number (+ n (data message 0))))
    (while (> key-number 128)(setf key-number (- key-number 12)))
    (while (minusp key-number)(setf key-number (+ key-number 12)))
    (setf (aref (data-array message) 0) key-number)
    message))

(defmethod invert ((message midi-key-message)(pivot t))
  (if pivot
      (let* ((key-number (invert (data message 0)(keynumber pivot))))
	(while (> key-number 128)(setf key-number (- key-number 12)))
	(while (minusp key-number)(setf key-number (+ key-number 12)))
	(setf (aref (data-array message) 0) key-number)
	message)
    message))

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

(defmethod midi-note-off-p ((message midi-note-off)) t)
(defmethod midi-note-on-p ((message midi-note-on)) t)
(defmethod midi-poly-pressure-p ((message midi-poly-pressure)) t)

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

(defmethod duplicate-channel-message ((message midi-note-on) &key channel data1 data2)
  (midi-note-on (1- (or channel (channel message)))
		(or data1 (data message 0))
		(or data2 (data message 1))))

(defmethod duplicate-channel-message ((message midi-note-off) &key channel data1 data2)
  (midi-note-off (1- (or channel (channel message)))
		 (or data1 (data message 0))
		 (or data2 (data message 1))))

(defmethod duplicate-channel-message ((message midi-poly-pressure) &key channel data1 data2)
  (midi-poly-pressure (1- (or channel (channel message)))
		      (or data1 (data message 0))
		      (or data2 (data message 1))))


;;; ---------------------------------------------------------------------- 
;;;			    MIDI-CONTROL-CHANGE
  
(defclass midi-control-change (midi-channel-message)
  ((command
    :initform +CONTROL-CHANGE+)
   (priority
    :initform 9)))

(defmethod midi-control-change-p ((message midi-control-change)) t)

(defun midi-control-change (channel-index controller-number value)
  "Creates instance of MIDI-CONTROL-CHANGE."
  (make-instance 'midi-control-change
		 :channel-index (assert-midi-channel-index channel-index)
		 :data (vector (assert-midi-data-value controller-number)
			       (assert-midi-data-value value))))

(defmethod duplicate-channel-message ((message midi-control-change) &key channel data1 data2)
  (midi-control-change (1- (or channel (channel message)))
		       (or data1 (data message 0))
		       (or data2 (data message 1))))

;;; ---------------------------------------------------------------------- 
;;;			   MIDI-CHANNEL-PRESSURE

(defclass midi-channel-pressure (midi-channel-message)
  ((command
    :initform +CHANNEL-PRESSURE+)
   (priority
    :initform 9)))

(defmethod midi-channel-pressure-p ((message midi-channel-pressure)) t)

(defmethod data-count ((message midi-channel-pressure)) 1)

(defun midi-channel-pressure (channel-index pressure)
  "Creates instance of MIDI-CHANNEL-PRESSURE."
  (make-instance 'midi-channel-pressure
		 :channel-index (assert-midi-channel-index channel-index)
		 :data (vector (assert-midi-data-value pressure) 0)))

(defmethod duplicate-channel-message ((message midi-channel-pressure) &key channel data1 data2)
  (declare (ignore data2))
  (midi-channel-pressure (1- (or channel (channel message)))
			 (or data1 (data message 0))))


;;; ---------------------------------------------------------------------- 
;;;			    MIDI-PROGRAM-CHANGE

(defclass midi-program-change (midi-channel-message)
  ((command
    :initform +PROGRAM-CHANGE+)
   (priority
    :initform 9)))

(defmethod midi-program-change-p ((message midi-program-change)) t)

(defmethod data-count ((message midi-program-change)) 1)

(defun midi-program-change (channel-index program-number)
  "Creates instance of MIDI-PROGRAM-CHANGE."
  (make-instance 'midi-program-change
		 :channel-index (assert-midi-channel-index channel-index)
		 :data (vector (assert-midi-data-value program-number) 0)))

(defmethod duplicate-channel-message ((message midi-program-change) &key channel data1 data2)
  (declare (ignore data2))
  (midi-program-change (1- (or channel (channel message)))
		       (or data1 (data message 0))))

;;; ---------------------------------------------------------------------- 
;;;			      MIDI-PITCH-BEND

(defclass midi-pitch-bend (midi-channel-message)
  ((command
    :initform +PITCH-BEND+)
   (priority
    :initform 9)))

(defmethod midi-pitch-bend-p ((message midi-pitch-bend)) t)

(defun midi-pitch-bend (channel-index low-byte high-byte)
  "Creates instance of MIDI-PITCH-BEND."
  (make-instance 'midi-pitch-bend
		 :channel-index (assert-midi-channel-index channel-index)
		 :data (vector (assert-midi-data-value low-byte)
			       (assert-midi-data-value high-byte))))

(defmethod duplicate-channel-message ((message midi-pitch-bend) &key channel data1 data2)
  (midi-pitch-bend (1- (or channel (channel message)))
		   (or data1 (data message 0))
		   (or data2 (data message 1))))


(defun event-p (object)
  "Predicate returns t if object is a valid MIDI event."
  (and (consp object)
       (numberp (car object))
       (midi-message-p (cdr object))))

(defun event-list-p (object)
  "Predicate returns t if object is a valid MIDI event list."
  (and (listp object)
       (every #'event-p object)))


(labels ((same-time-p (t0 t1 blur)
		      (<= (abs (- t1 t0)) blur))

	 (remove-non-notes (events)
			   (remove-if #'(lambda (event)
					  (not (midi-note-on-p (cdr event))))
				      events))

	 (pull-chord (events blur)
		     (let* ((ev0 (car events))
			    (t0 (car ev0))
			    (acc (list ev0))
			    (i 1))
		       (while (and (< i (length events))
				   (same-time-p t0 (car (nth i events)) blur))
			 (push (nth i events) acc)
			 (setf i (1+ i)))
		       (reverse acc)))

	 (flatten-non-chords (keylist)
			     (if (not keylist)
				 nil
			       (let ((head (car keylist)))
				 (if (= (length head) 1)
				     (setf head (car head)))
				 (cons head (flatten-non-chords (cdr keylist))))))

	 (from-event-list (events blur)
			  (setf events (remove-non-notes (sort-midi-events events)))
			  ;; group events by chord
			  (let ((chords '())
				(acc '()))
			    (while events
			      (let ((chord (pull-chord events blur)))
				(dolist (ev chord)
				  (setf events (remove ev events)))
				(push chord chords)))
			    ;; extract keynumbers
			    (dolist (chord chords)
			      (push (mapcar #'(lambda (q)(data (cdr q) 0)) chord) acc))
			    (flatten-non-chords acc))) )
			   
	(defmethod keylist ((lst list) &key (blur 0))
	  (cond ((event-list-p lst)
		 (from-event-list lst blur))
	   
		((every #'midi-message-p lst)
		 (remove nil (loop for msg in lst collect
				   (if (midi-note-on-p msg)
				       (data msg 0)))))
	   
		(t (remove-if #'(lambda (q)
				  (or (and (numberp q)(minusp q))
				      (null q)))
			      (keynumber lst))))) )
