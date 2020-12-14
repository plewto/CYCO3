;;;; CYCO midi midi-util.lisp
;;;;
;;;; Low level MIDI functions.
;;;;

(in-package :cyco)


;;; ************************************************************************
;;;                     MIDI variable length values (vlv)
;;;                                      
;;; A variable length value uses the low order 7 bits of a byte to
;;; represent the value or part of the value. The high order bit is an
;;; "escape" or "continuation" bit. All but the last byte of a variable
;;; length value have the high order bit set. The last byte has the high
;;; order bit cleared. The bytes appear most significant byte first.
;;; 

(defun int->midi-vlv (value)
  "Converts int to MIDI variable-length value.
Returns list."
  (let ((bytes (list (logand value #x7f)))
	(n (ash value -7)))
    (while (plusp n)
      (push (+ #x80 (logand n #x7f)) bytes)
      (setf n (ash n -7)))
    bytes))

(defun midi-vlv->int (mbv)
  "Converts list holding MIDI variable-length value to int."
  (let ((result 0)
	(scale 1))
    (dolist (byte (reverse mbv))
      (setf result (+ result (* scale (logand byte #x7f))))
      (setf scale (* scale 128)))
    result))

(defun read-midi-vlv (array offset)
  "Reads MIDI variable-length value from array and converts to int.
array  - the array
offset - location in array.
Returns cons (value . byte-count)"
  (let* ((byte (aref array offset))
	 (acc (list byte))
	 (counter 0))
    (while (plusp (logand byte #x80))
      (setf offset (1+ offset))
      (setf byte (aref array offset))
      (push byte acc)
      (setf counter (1+ counter)))
    (cons (midi-vlv->int (reverse acc)) counter)))

(defun read-midi-long (array offset)
  "Reads 4 bytes from array starting at offset.
Returns cons (value 4)."
  (let* ((b0 (aref array offset))
	 (b1 (aref array (1+ offset)))
	 (b2 (aref array (2+ offset)))
	 (b3 (aref array (3+ offset)))
	 (value (+ (ash 24 (logand b0 #xFF))
		   (ash 16 (logand b1 #xFF))
		   (ash  8 (logand b2 #xFF))
		   (logand b3 #xFF))))
    (cons value 4)))


;;; ********************************************************************** 
;;;			     MIDI Bend values
;;;
;;; MIDI bend is 14-bits specified as lower 7-bits of two bytes.
;;; The high bits are always cleared with byte order lsb msb.
;;;

(defun bend->midi-data (normalized-bend)
  "Converts signed normalized value (-1..+1) to 14-bit MIDI bend value.
Returns vector #(lsb msb)"
  (let* ((v (truncate (+ (* (min normalized-bend 0.9999) 8192) 8192)))
	 (lsb (logand v #x7f))
	 (msb (logand (ash v -7) #x7f)))
    (vector lsb msb)))

(defun midi-data->bend (low-byte high-byte)
  "Converts 14-bit MIDI bend data to normalized float (-1..+1)."
  (let* ((b14 (+ (ash high-byte 7) low-byte))
	 (bend (- (* 1/8192 b14) 1.0))
	 (scale 1000.0))
    (/ (round (* bend scale)) scale)))

(defun read-midi-bend (array offset)
  "Reads MIDI bend data from array.
array  - the array
offset - location in array
Returns cons (value . new-offset)
where value is float, a normalized bend (-1..+1)
and new-offset is location in array after bend data."
  (let* ((low-byte (aref array offset))
	 (high-byte (aref array (1+ offset)))
	 (bend (midi-data->bend low-byte high-byte)))
    (cons bend (+ 2 offset))))

;;; ********************************************************************** 
;;;			     MIDI Data Values
;;;
;;; MIDI data uses lower 7-bits of a byte.  The high bit is always clear
;;;

(defun norm->midi-data (normalized-value)
  "Converts unsigned normalized value (0..1) to MIDI data (0..127)
Returns int."
  (let ((v (limit (truncate (* normalized-value 127)) 0 127)))
    v))

(defun midi-data->norm (data-byte)
  "Converts 7-bit MIDI data value to normalized float.
Returns float (0..1)"
  (let ((scale 1000.0)
	(norm (/ data-byte 127.0)))
    (/ (round (* norm scale)) scale)))

(defun read-midi-data (array offset)
  "Reads single MIDI data byte from array.
array - the array
offset - location in array to read.
Returns cons (n . new-offset)
where n is float (0..1)
and new-offset is location in array after data."
  (let ((byte (aref array offset))
	(scale 10000.0))
    (cons (/ (round (* (/ byte 127.0) scale)) scale)
	  (1+ offset))))

(defun signed-norm->midi-data (signed-normal-value)
  "Converts signed normalized value to 7-bit MIDI data.
f(-1) -->   0
f(0)  -->  63
f(+1) --> 127"
  (let ((data-byte (+ (* 127/2 signed-normal-value) 127/2)))
    (limit (truncate data-byte) 0 127)))

(defun read-signed-midi-data (array offset)
  "Reads single 7-bit MIDI byte as signed normalized value.
array - array.
offset - location in array.
Returns cons (n . new-offset)
where n is signed normalized float (-1..+1)
and new-offset is location in array after data byte."
  (let ((byte (aref array offset))
	(scale 10000.0))
    (cons (/ (round (* (- (* 2/127 byte) 1.0) scale)) scale)
	  (1+ offset))))

(defun bpm->beat-period (tempo-bpm)
  "Returns beat duration in seconds for given tempo.
bpm - tempo in beats per minute.
Returns number."
  (/ 60 tempo-bpm))


(defun bpm->microseconds (tempo-bpm)
  (truncate (* 1e6 (bpm->beat-period tempo-bpm))))

(defmethod tick-duration ((tempo-bpm number) &key (unit 'q))
  (let ((scale (cond ((eq unit 'q) 1)
		     ((eq unit 'h) 2)
		     ((eq unit 'w) 4)
		     ((eq unit 'e) 0.5)
		     ((eq unit 's) 0.25)
		     (t 1))))
    (/ (* scale 60.0)
       (* tempo-bpm +TICKS-PER-BEAT+))))

;;; **********************************************************************
;;;                  MIDI message/event predicates
;;;

(defun midi-message-match-p (message-1 message-2)
  "Predicate, true if MIDI messages are identical."
  (and (midi-message-p message-1)
       (midi-message-p message-2)
       (= (command message-1)
	  (command message-2))
       (if (midi-channel-message-p message-1)
	   (= (channel-index message-1)
	      (channel-index message-2))
	 t)
       (let ((data-match-flag t))
	 (dotimes (index (data-count message-1))
	   (setf data-match-flag (and data-match-flag (= (data message-1 index)
							 (data message-2 index)))))
	 data-match-flag)))

(defun midi-event-match-p (event-1 event-2 &key (time-fuzz 0.00001))
  "Predicate, true if MIDI events are identical.

An 'event' is defined as (cons time midi-message).
Event times are considered identical if their difference is less then time-fuzz."
  (and (consp event-1)
       (consp event-2)
       (let ((t1 (car event-1))
	     (t2 (car event-2)))
	 (and (numberp t1)
	      (numberp t2)
	      (<= (abs (- t1 t2)) time-fuzz)))
       (midi-message-match-p (cdr event-1)(cdr event-2))))
       
(labels ((compare-event-list
	  (event-list-1 event-list-2 time-fuzz)
	  (if (not event-list-1)
	      t
	    (and (midi-event-match-p (car event-list-1)
				     (car event-list-2) :time-fuzz time-fuzz)
		 (compare-event-list (cdr event-list-1)
				     (cdr event-list-2) time-fuzz)))))

  (defun midi-event-list-match-p (event-list-1 event-list-2 &key (time-fuzz 0.00001))
    "Predicate, true if two midi event-list are identical.
Event times are considered identical if their difference is less then time-fuzz."
    (and (listp event-list-1)
	 (listp event-list-2)
	 (= (length event-list-1)
	    (length event-list-2))
	 (compare-event-list event-list-1 event-list-2 time-fuzz))))


(defun thin-controller-events (midi-events channel-index)
  "Removes duplicate controller events."
  (let ((acc '())
	(current-values (->vector (copies 128 -1))))
    (dolist (event midi-events)
      (let ((message (cdr event)))
	(if (and (midi-control-change-p message)
		 (= (channel-index message) channel-index))
	    (let ((controller-number (data message 0))
		  (value (data message 1)))
	      (if (not (= (aref current-values controller-number) value))
		  (progn
		    (setf (aref current-values controller-number) value)
		    (push event acc))))
	  (push event acc))))
    (sort-midi-events acc)))


