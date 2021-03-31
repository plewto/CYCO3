;;;; CYCO midi meta.lisp
;;;;
;;;; Low level MIDI meta messages.
;;;;
;;;; Not all meta messages are defined.
;;;; Implemented:
;;;;    text, copyright, track-name, instrument-name, lyric, cue, marker
;;;;    end-of-track (as singleton)
;;;;    tempo
;;;;    signature
;;;;    key-signature (partially)
;;;;
;;;; Not implemented:
;;;;    device-name
;;;;    channel-prefix
;;;;    port
;;;;    smpte-offset
;;;;    sequencer-specific
;;;;

(in-package :cyco)

(defclass midi-meta-message (midi-message)
  ((command
    :initform +META+)
   (meta-type
    :type integer
    :reader meta-type
    :initform 0
    :initarg :meta-type)
   (priority
    :initform 0)))

(defmethod midi-meta-message-p ((message midi-meta-message)) t)

(defmethod ->string ((object midi-meta-message))
  (sformat  "META ~9A " (gethash (meta-type object) +MNEMONICS+)))

;;; ---------------------------------------------------------------------- 
;;;			      MIDI-META-TEXT

(defclass midi-meta-text (midi-meta-message)
  ((meta-type
    :initform +TEXT-MESSAGE+)
   (text
    :type string
    :reader text
    :initform ""
    :initarg :text)))

(defclass midi-meta-copyright (midi-meta-text)
  ((meta-type
    :initform +COPYRIGHT+)))

(defclass midi-meta-track-name (midi-meta-text)
  ((meta-type
    :initform +TRACK-NAME+)))

(defclass midi-meta-instrument-name (midi-meta-text)
  ((meta-type
    :initform +INSTRUMENT-NAME+)))

(defclass midi-meta-lyric (midi-meta-text)
  ((meta-type
    :initform +LYRIC-TEXT+)))

(defclass midi-meta-cue (midi-meta-text)
  ((meta-type
    :initform +CUE-POINT+)
   (priority
    :initform 1)))

(defclass midi-meta-marker (midi-meta-text)
  ((meta-type
    :initform +MARKER-TEXT+)))

(defmethod midi-meta-text-p ((object midi-meta-text)) t)
(defmethod midi-meta-copyright-p ((object midi-meta-copyright)) t)
(defmethod midi-meta-track-name-p ((object midi-meta-track-name)) t)
(defmethod midi-meta-instrument-name-p ((object midi-meta-instrument-name)) t)
(defmethod midi-meta-lyric-p ((object midi-meta-lyric)) t)
(defmethod midi-meta-cue-p ((object midi-meta-cue)) t)
(defmethod midi-meta-marker-p ((object midi-meta-marker)) t)

(defun midi-meta-copyright (text)
  (make-instance 'midi-meta-copyright :text (->string text)))

(defun midi-meta-track-name (text)
  (make-instance 'midi-meta-track-name :text (->string text)))

(defun midi-meta-instrument-name (text)
  (make-instance 'midi-meta-instrument-name :text (->string text)))

(defun midi-meta-lyric (text)
  (make-instance 'midi-meta-lyric :text (->string text)))

(defun midi-meta-cue (text)
  (make-instance 'midi-meta-cue :text (->string text)))

(defun midi-meta-marker (text)
  (make-instance 'midi-meta-marker :text (->string text)))

(defmethod data-count ((message midi-meta-text))
  (length (text message)))

(defmethod ->string ((message midi-meta-text))
  (str+ (call-next-method)
	(sformat  "~s" (text message))))

(defun midi-meta-text (text)
  (make-instance 'midi-meta-text
		 :meta-type +TEXT-MESSAGE+
		 :text (->string text)))

(defmethod render-midi-message ((message midi-meta-text))
  (let ((count (data-count message))
	(bytes '()))
    (dotimes (i count)
      (push (char-code (char (text message) i)) bytes))
    (append (list +META+
		  (meta-type message))
	    (int->midi-vlv count)
	    (reverse bytes))))

;;; ---------------------------------------------------------------------- 
;;;			  MIDI-META-END-OF-TRACK (Singleton)
  
(defclass %midi-end-of-track% (midi-meta-message)
  ((meta-type
    :initform +END-OF-TRACK+)
   (priority
    :initform 99)))

(defmethod midi-end-of-track-p ((object %midi-end-of-track%)) t)

(defmethod render-midi-message ((message %midi-end-of-track%))
  '(#xFF #x2F 0))

(constant-function midi-end-of-track
		   (make-instance '%midi-end-of-track%))

;;; ---------------------------------------------------------------------- 
;;;			     MIDI-TEMPO-CHANGE

(defclass midi-tempo-message (midi-meta-message)
  ((meta-type
    :initform +TEMPO-CHANGE+)
   (priority
    :initform 1)
   (tempo
    :type float
    ;;:reader tempo
    :initform 120.0
    :initarg :bpm)))

(defmethod midi-tempo-message-p ((object midi-tempo-message)) t)

(defun midi-tempo-message (tempo-bpm)
  (make-instance 'midi-tempo-message :bpm (float tempo-bpm)))

(defmethod ->string ((message midi-tempo-message))
  (str+ (call-next-method)
	(sformat  "BPM: ~A" (slot-value message 'tempo))))

(defmethod tick-duration ((message midi-tempo-message) &key (unit 'q))
  (tick-duration (slot-value message 'tempo) :unit unit))

(defmethod render-midi-message ((message midi-tempo-message))
  (let* ((usec (bpm->microseconds (slot-value message 'tempo)))
	 (d0 (logand (ash usec -16) #xFF))
	 (d1 (logand (ash usec -8) #xFF))
	 (d2 (logand usec #xFF)))
    (list +META+ +TEMPO-CHANGE+ 3 d0 d1 d2)))
	     
;;; ---------------------------------------------------------------------- 
;;;			    MIDI-TIME-SIGNATURE

(defclass midi-time-signature (midi-meta-message)
  ((meta-type
    :initform +TIME-SIGNATURE+)
   (priority
    :initform 1)
   (numerator
    :type integer
    :reader timesig-numerator
    :initform 4
    :initarg :num)
   (unit
    :type symbol
    :reader timesig-unit
    :initform 'q
    :initarg :unit)
   (tpq
    :type integer
    :initform 24)
   (met
    :type integer
    :initform 8)))

(defmethod midi-time-signature-p ((object midi-time-signature)) t)


(flet ((map-timesig-unit (sym)
			 (cond ((eq sym 'q) 2)
			       ((eq sym 'w) 0)
			       ((eq sym 'h) 1)
			       ((eq sym 'e) 3)
			       ((eq sym 's) 4)
			       (t (cyco-value-error 'midi-time-signature sym) 2))))
  
  (defun midi-time-signature (beats-per-bar beat-unit)
    (map-timesig-unit beat-unit)
    (make-instance 'midi-time-signature
		   :num beats-per-bar
		   :unit beat-unit))
  
  (defmethod ->string ((message midi-time-signature))
    (str+ (call-next-method)
	  (sformat  "~A/~A"
		    (timesig-numerator message)
		    (timesig-unit message))))
  
  (defmethod render-midi-message ((message midi-time-signature))
    (list +META+ +TIME-SIGNATURE+ 4
	  (timesig-numerator message)
	  (map-timesig-unit (timesig-unit message))
	  (slot-value message 'tpq)
	  (slot-value message 'met))))


;;; ---------------------------------------------------------------------- 
;;;			    MIDI-KEY-SIGNATURE
;;;
;;; ISSUE: MIDI-KEY-SIGNATURE is not fully implemented.
;;; specifically not sure hoe to format negative numbers for sf (number of
;;; sharps/flats) field.   It should be fine for positive sf values.
;;;

(defclass midi-key-signature (midi-meta-message)
  ((meta-type
    :initform +KEY-SIGNATURE+)
   (priority
    :initform 1)
   (sf					; number sharps 0..+7
    :type integer			;        flats -7..-1
    :initform 0
    :initarg :sf)
   (mi					; major = 0
    :type integer			; minor = 1
    :initform 0
    :initarg :min)))

(defmethod midi-key-signature-p ((object midi-key-signature)) nil)


(defun midi-key-signature (sharps-or-flats &optional (minor nil))
  "sharps-or-flats positive 0..+7 for sharps, negative -7..-1 for flats."  
  (make-instance 'midi-key-signature
		 :sf sharps-or-flats
		 :min (if minor 1 0)))

(defmethod ->string ((message midi-key-signature))
  (let ((sharps-or-flats (slot-value message 'sf))
	(is-minor (slot-value message 'mi)))
    (str+ (call-next-method)
	  (if (minusp sharps-or-flats)
	      (sformat  "~A flats " (abs sharps-or-flats))
	    (sformat  "~A sharps " sharps-or-flats))
	  (if (zerop is-minor) "major" "minor"))))

(defmethod render-midi-message ((message midi-key-signature))
  (list +META+ +KEY-SIGNATURE+ 2 (slot-value message 'sf)(slot-value message 'mi))) 



