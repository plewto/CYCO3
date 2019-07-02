;;;; CYCO
;;;;
;;;; Low level MIDI functions.
;;;;

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
  (let ((acc (list (logand value #x7f)))
	(n (ash value -7)))
    (while (plusp n)
      (push (+ #x80 (logand n #x7f)) acc)
      (setf n (ash n -7)))
    acc))

(defun midi-vlv->int (mbv)
  "Converts list holding MIDI variable-length value to int."
  (let ((acc 0)
	(scale 1))
    (dolist (byte (reverse mbv))
      (setf acc (+ acc (* scale (logand byte #x7f))))
      (setf scale (* scale 128)))
    acc))

(defun read-midi-vlv (ary offset)
  "Reads MIDI variable-length value from array and converts to int.
ary - array
offset - location in array.
Returns cons (value . byte-count)"
  (let* ((byte (aref ary offset))
	 (acc (list byte))
	 (counter 0))
    (while (plusp (logand byte #x80))
      (setf offset (1+ offset))
      (setf byte (aref ary offset))
      (push byte acc)
      (setf counter (1+ counter)))
    (cons (midi-vlv->int (reverse acc)) counter)))

(defun read-midi-long (ary offset)
  "Reads 4 bytes from array starting at offset.
Returns cons (value 4)."
  (let* ((b0 (aref ary offset))
	 (b1 (aref ary (1+ offset)))
	 (b2 (aref ary (2+ offset)))
	 (b3 (aref ary (3+ offset)))
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

(defun bend->midi-data (bnd)
  "Converts signed normalized value (-1..+1) to 14-bit MIDI bend value.
Returns vector #(lsb msb)"
  (let* ((v (truncate (+ (* (min bnd 0.9999) 8192) 8192)))
	 (lsb (logand v #x7f))
	 (msb (logand (ash v -7) #x7f)))
    (vector lsb msb)))

(defun midi-data->bend (lsb msb)
  "Converts 14-bit MIDI bend data to normalized float (-1..+1)."
  (let* ((b14 (+ (ash msb 7) lsb))
	 (bend (- (* 1/8192 b14) 1.0))
	 (scale 1000.0))
    (/ (round (* bend scale)) scale)))

(defun read-midi-bend (ary offset)
  "Reads MIDI bend data from array.
ary - array
offset - location in array
Returns cons (value . new-offset)
where value is float, a normalized bend (-1..+1)
and new-offset is location in array after bend data."
  (let* ((lsb (aref ary offset))
	 (msb (aref ary (1+ offset)))
	 (bend (midi-data->bend lsb msb)))
    (cons bend (+ 2 offset))))

;;; ********************************************************************** 
;;;			     MIDI Data Values
;;;
;;; MIDI data uses lower 7-bits of a byte.  The high bit is always clear
;;;

(defun norm->midi-data (n)
  "Converts unsigned normalized value (0..1) to MIDI data (0..127)
Returns int."
  (let ((v (limit (truncate (* n 127)) 0 127)))
    v))

(defun midi-data->norm (d)
  "Converts 7-bit MIDI data value to normalized float.
Returns float (0..1)"
  (let ((scale 1000.0)
	(norm (/ d 127.0)))
    (/ (round (* norm scale)) scale)))

(defun read-midi-data (ary offset)
  "Reads single MIDI data byte from array.
ary - array
offset - location in array to read.
Returns cons (n . new-offset)
where n is float (0..1)
and new-offset is location in array after data."
  (let ((byte (aref ary offset))
	(scale 10000.0))
    (cons (/ (round (* (/ byte 127.0) scale)) scale)
	  (1+ offset))))

(defun signed-norm->midi-data (n)
  "Converts signed normalized value to 7-bit MIDI data.
f(-1) -->   0
f(0)  -->  63
f(+1) --> 127"
  (let ((v (+ (* 127/2 n) 127/2)))
    (limit (truncate v) 0 127)))

(defun read-signed-midi-data (ary offset)
  "Reads single 7-bit MIDI byte as signed normalized value.
ary - array.
offset - location in array.
Returns cons (n . new-offset)
where n is signed normalized float (-1..+1)
and new-offset is location in array after data byte."
  (let ((byte (aref ary offset))
	(scale 10000.0))
    (cons (/ (round (* (- (* 2/127 byte) 1.0) scale)) scale)
	  (1+ offset))))

(defun bpm->beat-period (bpm)
  "Returns beat duration in seconds for given tempo.
bpm - tempo in beats per minute.
Returns number."
  (/ 60 bpm))

;; (defmethod bpm->microseconds ((bpm number))
;;   (truncate (* 1e6 (bpm->beat-period bpm))))

(defun bpm->microseconds (bpm)
  (truncate (* 1e6 (bpm->beat-period bpm))))

(defmethod tick-duration ((bpm number) &key (unit 'q))
  (let ((scale (cond ((eq unit 'q) 1)
		     ((eq unit 'h) 2)
		     ((eq unit 'w) 4)
		     ((eq unit 'e) 0.5)
		     ((eq unit 's) 0.25)
		     (t 1))))
    (/ (* scale 60.0)
       (* bpm *TICKS-PER-BEAT*))))




