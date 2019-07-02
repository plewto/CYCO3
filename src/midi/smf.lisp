;;;; CYCO
;;;; smf ~ Standard Midi File
;;;;

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
		 (division +TICKS-PER-BEAT+))
  "Creates instance of SMF (Standard MIDI File).
format - file format (only type 1 currently supported).
track-count - number of tracks (only single track currently supported).
division - number of ticks per beat. Defaults to +TICKS-PER-BEAT+"
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

(defmethod ->string ((obj smf))
  (sformat "SMF format: ~A,  division: ~A,  tracks: ~A"
	   (smf-format (smf-header obj))
	   (smf-division (smf-header obj))
	   (length (smf-tracks obj))))

(defmethod clone ((src smf) &key new-name new-parent)
  (dismiss new-name new-parent)
  (let* ((frm (smf-format (smf-header src)))
	 (div (smf-division (smf-header src)))
	 (tracks (clone (smf-tracks src)))
	 (other (smf :format frm
		     :track-count (length tracks)
		     :division div)))
    (setf (slot-value other 'tracks) tracks)
    other))

(defmethod smf-track-count ((obj smf))
  (length (smf-tracks obj)))

(defmethod smf-track ((obj smf) &optional (index 0))
  (aref (smf-tracks obj) index))

(defmethod smf-track! ((obj smf)(trk smf-track) &optional (index 0))
  (setf (aref (smf-tracks index)) trk))

(defmethod render-smf ((obj smf) &key (pad 1.0))
  (let* ((tcount (smf-track-count obj))
	 (acc (render-smf-header (smf-header obj) tcount))
	 (tracks (smf-tracks obj)))
    (dotimes (i tcount)
      (setf acc (append acc (render-smf-track (aref tracks i) pad))))
    acc))

(defmethod write-smf ((obj smf)(filename string) &key (pad 1.0)(no-overwrite nil))
  (format t "Writing SMF file ~S~%" filename)
  (let ((data (render-smf obj :pad pad))
	(strm (open filename
		    :direction :output
		    :if-exists (if no-overwrite nil :supersede)
		    :element-type '(unsigned-byte 8))))
    (dolist (b data)
      (write-byte b strm))
    (close strm)
    obj))

(defmethod dump-events ((obj smf) &key range (filter #'false)(render nil))
  (dotimes (i (smf-track-count obj))
    (let ((trk (smf-track obj i)))
      (format t "TRACK: ~s~%" (name trk))
      (dump-events trk :range range :filter filter :render render)))
  (smf-track-count obj))





