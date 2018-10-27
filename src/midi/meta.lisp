;;;; CYCO3 src/midi/meta
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

(defmethod midi-meta-message-p ((obj t)) nil)
(defmethod midi-meta-message-p ((evn midi-meta-message)) t)

(defmethod ->string ((obj midi-meta-message))
  (sformat  "META ~9A " (gethash (meta-type obj) +MNEMONICS+)))

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

(defmethod midi-meta-text-p ((obj t)) nil)
(defmethod midi-meta-text-p ((obj midi-meta-text)) t)
(defmethod midi-meta-copyright-p ((obj t)) nil)
(defmethod midi-meta-copyright-p ((obj midi-meta-copyright)) t)
(defmethod midi-meta-track-name-p ((obj t)) nil)
(defmethod midi-meta-track-name-p ((obj midi-meta-track-name)) t)
(defmethod midi-meta-instrument-name-p ((obj t)) nil)
(defmethod midi-meta-instrument-name-p ((obj midi-meta-instrument-name)) t)
(defmethod midi-meta-lyric-p ((obj t)) nil)
(defmethod midi-meta-lyric-p ((obj midi-meta-lyric)) t)
(defmethod midi-meta-cue-p ((obj t)) nil)
(defmethod midi-meta-cue-p ((obj midi-meta-cue)) t)
(defmethod midi-meta-marker-p ((obj t)) nil)
(defmethod midi-meta-marker-p ((obj midi-meta-marker)) t)

(defun midi-meta-copyright (s)
  (make-instance 'midi-meta-copyright :text (->string s)))

(defun midi-meta-track-name (s)
  (make-instance 'midi-meta-track-name :text (->string s)))

(defun midi-meta-instrument-name (s)
  (make-instance 'midi-meta-instrument-name :text (->string s)))

(defun midi-meta-lyric (s)
  (make-instance 'midi-meta-lyric :text (->string s)))

(defun midi-meta-cue (s)
  (make-instance 'midi-meta-cue :text (->string s)))

(defun midi-meta-marker (s)
  (make-instance 'midi-meta-marker :text (->string s)))

(defmethod data-count ((evn midi-meta-text))
  (length (text evn)))

(defmethod ->string ((evn midi-meta-text))
  (str+ (call-next-method)
	(sformat  "~s" (text evn))))

(defun midi-meta-text (txt)
  (make-instance 'midi-meta-text
		 :meta-type +TEXT-MESSAGE+
		 :text (->string txt)))

(defmethod render-midi-message ((evn midi-meta-text))
  (let ((count (data-count evn))
	(acc '()))
    (dotimes (i count)
      (push (char-code (char (text evn) i)) acc))
    (append (list +META+
		  (meta-type evn))
	    (int->midi-vlv count)
	    (reverse acc))))

;;; ---------------------------------------------------------------------- 
;;;			  MIDI-META-END-OF-TRACK (Singleton)
  
(defclass %midi-end-of-track% (midi-meta-message)
  ((meta-type
    :initform +END-OF-TRACK+)
   (priority
    :initform 99)))

(defmethod midi-end-of-track-p ((obj t)) nil)
(defmethod midi-end-of-track-p ((obj %midi-end-of-track%)) t)

(defmethod render-midi-message ((evn %midi-end-of-track%))
  '(#xFF #x2F 0))

(constant-function midi-end-of-track
		   (make-instance '%midi-end-of-track%))

;;; ---------------------------------------------------------------------- 
;;;			     MIDI-TEMPO-CHANGE

(defclass midi-tempo-message (midi-meta-message)
  ((meta-type
    :initform +TEMPO-CHANGE+)
   (priority
    :initform 2)
   (tempo
    :type float
    ;;:reader tempo
    :initform 120.0
    :initarg :bpm)))

(defmethod midi-tempo-message-p ((obj t)) nil)
(defmethod midi-tempo-message-p ((obj midi-tempo-message)) t)

(defun midi-tempo-message (bpm)
  (make-instance 'midi-tempo-message :bpm (float bpm)))

(defmethod ->string ((evn midi-tempo-message))
  (str+ (call-next-method)
	(sformat  "BPM: ~A" (slot-value evn 'tempo))))

(defmethod tick-duration ((evn midi-tempo-message) &key (unit 'q))
  (tick-duration (slot-value evn 'tempo) :unit unit))

(defmethod render-midi-message ((evn midi-tempo-message))
  (let* ((usec (bpm->microseconds (slot-value evn 'tempo)))
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
    :initform +TICKS-PER-BEAT+)
   (met
    :type integer
    :initform 8)))

(defmethod midi-time-signature-p ((obj t)) nil)
(defmethod midi-time-signature-p ((obj midi-time-signature)) t)

(defun .map-midi-timesig-unit. (sym)
  (cond ((eq sym 'q) 2)
	((eq sym 'w) 0)
	((eq sym 'h) 1)
	((eq sym 'e) 3)
	((eq sym 's) 4)
	(t (cyco-value-error 'midi-time-signature sym) 2)))

(defun midi-time-signature (num unit)
  (.map-midi-timesig-unit. unit)	; validate unit
  (make-instance 'midi-time-signature
		 :num num
		 :unit unit))

(defmethod ->string ((evn midi-time-signature))
  (str+ (call-next-method)
	(sformat  "~A/~A"
		(timesig-numerator evn)
		(timesig-unit evn))))

(defmethod render-midi-message ((evn midi-time-signature))
  (list +META+ +TIME-SIGNATURE+ 4
	(timesig-numerator evn)
	(.map-midi-timesig-unit. (timesig-unit evn))
	(slot-value evn 'tpq)
	(slot-value evn 'met)))


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

(defmethod midi-key-signature-p ((obj t)) nil)
(defmethod midi-key-signature-p ((obj midi-key-signature)) nil)

(defun midi-key-signature (sf &optional (minor nil))
  (make-instance 'midi-key-signature
		 :sf sf
		 :min (if minor 1 0)))

(defmethod ->string ((evn midi-key-signature))
  (let ((sf (slot-value evn 'sf))
	(mi (slot-value evn 'mi)))
    (str+ (call-next-method)
	  (if (minusp sf)
	      (sformat  "~A flats " (abs sf))
	    (sformat  "~A sharps " sf))
	  (if (zerop mi) "major" "minor"))))

(defmethod render-midi-message ((evn midi-key-signature))
  (list +META+ +KEY-SIGNATURE+ 2 (slot-value evn 'sf)(slot-value evn 'mi))) 

