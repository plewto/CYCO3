;;;; CYCO midi smf.lisp
;;;;
;;;; Defines MIDI file   smf ~ Standard MIDI File
;;;;

(in-package :cyco)

(defclass smf nil
  ((header
    :type smf-header
    :reader smf-header
    :initform (make-instance 'smf-header)
    :initarg :header)
   (tracks
    :type vector
    :reader smf-tracks
    :initform #()
    :initarg :tracks)))

(defun smf (&key (format 1)
		 (track-count 1)
		 (division *TICKS-PER-BEAT*))
  "Creates instance of SMF (Standard MIDI File).
format      - file format (only type 1 currently supported).
track-count - number of tracks (only single track currently supported).
division    - number of ticks per beat. Defaults to *TICKS-PER-BEAT*"
  (make-instance 'smf
		 :header (make-instance 'smf-header
					:format format
					:division division)
		 :tracks (let ((acc '()))
			   (dotimes (i track-count)
			     (push (make-instance 'smf-track
						  :name (sformat "Track-~A" i))
				   acc))
			   (->vector (reverse acc)))))

(defmethod ->string ((midi-file smf))
  (sformat "SMF format: ~A,  division: ~A,  tracks: ~A"
	   (smf-format (smf-header midi-file))
	   (smf-division (smf-header midi-file))
	   (length (smf-tracks midi-file))))

(defmethod clone ((mother smf) &key &allow-other-keys)
  (let* ((frm (smf-format (smf-header mother)))
	 (div (smf-division (smf-header mother)))
	 (tracks (clone (smf-tracks mother)))
	 (daughter (smf :format frm
			:track-count (length tracks)
			:division div)))
    (setf (slot-value daughter 'tracks) tracks)
    daughter))

(defmethod smf-track-count ((midi-file smf))
  (length (smf-tracks midi-file)))

(defmethod smf-track ((midi-file smf) &optional (index 0))
  (aref (smf-tracks midi-file) index))

(defmethod set-smf-track ((midi-file smf)(track smf-track) &optional (index 0))
  (setf (aref (smf-tracks midi-file) index) track))

(defmethod render-smf ((midi-file smf) &key (pad 1.0))
  (let* ((tcount (smf-track-count midi-file))
	 (bytes (render-smf-header (smf-header midi-file) tcount))
	 (tracks (smf-tracks midi-file)))
    (dotimes (i tcount)
      (setf bytes (append bytes (render-smf-track (aref tracks i) pad))))
    bytes))

(defmethod write-smf ((midi-file smf)(filename string) &key (pad 1.0)(no-overwrite nil))
  (format t "Writing SMF file ~S~%" filename)
  (let ((bytes (render-smf midi-file :pad pad))
	(strm (open filename
		    :direction :output
		    :if-exists (if no-overwrite nil :supersede)
		    :element-type '(unsigned-byte 8))))
    (dolist (byte bytes)
      (write-byte byte strm))
    (close strm)
    midi-file))

(defmethod dump-events ((midi-file smf) &key range (filter #'false)(render nil))
  (dotimes (track-number (smf-track-count midi-file))
    (let ((track (smf-track midi-file track-number)))
      (format t "TRACK: ~s~%" (name track))
      (dump-events track :range range :filter filter :render render)))
  (smf-track-count midi-file))
