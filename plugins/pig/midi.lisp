;;;; CYCO pig plugin midi.lisp
;;;;
;;;; Provides sending individual MIDI messages to a Pigiron operator.
;;;; The global variable pig:*midi-op* holds the name of the target
;;;; operator.
;;;;

(in-package :pig)

(global *midi-op* "out.1")

(labels ((midi->pig (&rest bytes)
		    (let ((acc (sformat "midi, ~A, " *midi-op*)))
		      (dolist (b bytes)
			(setf acc (sformat "~A ~A," acc b)))
		      (setf acc (subseq acc 0 (1- (length acc))))
		      (osc-send "exec" acc)))
	 
	 (->status (channel stat)
		   (let ((ci (logand (1- channel) #x0F)))
		     (logior stat ci))))
	
	
	(defun note-off (channel key &optional (velocity 0))
	  "(pig:note-off) sends note-off to Pigiron operator named by pig:*midi-op*"
	  (let ((status (->status channel #x80))
		(vel (truncate (logand velocity #x7F))))
	    (midi->pig status key vel)))

	(defun note-on (channel key &optional (velocity #x40))
	  "(pig:note-on) sends note-on to Pigiron operator named by pig:*midi-op*"
	  (let ((status (->status channel #x90))
		(vel (truncate (logand velocity #x7F))))
	    (midi->pig status key vel)))
	
	(defun poly-pressure (channel key pressure)
	  "(pig:poly-pressure) sends polyphonic pressure to Pigiron operator named by pig:*midi-op*"
	  (let ((status (->status channel #xA0)))
	    (midi->pig status (logand key #x7F) (logand pressure #x7F))))

	(defun controller (channel ctrl value)
	  "(pig:controller) sends control-change to Pigiron operator named by pig:*midi-op*"
	  (let ((status (->status channel #xB0)))
	    (midi->pig status (logand ctrl #x7F)(logand value #x7F))))

	(defun program (channel number)
	  "(pig:program) sends program-change to Pigiron operator named by pig:*midi-op*"
	  (let ((status (->status channel #xC0)))
	    (midi->pig status (logand number #x7F))))

	(defun mono-pressure (channel pressure)
	  "(pig:mono-pressure) sends channel-pressure to Pigiron operator named by pig:*midi-op*"
	  (let ((status (->status channel #xD0)))
	    (midi->pig status (logand pressure #x7F))))

	(defun bend (channel normal-bend)
	  "(pig:bend) sends pitch-bend to Pigiron operator named by pig:*midi-op*
bend argument is normalized -1.0 <= bend <= +1.0"
	  (let ((status (->status channel #xE0))
		 (bytes (cyco:bend->midi-data normal-bend)))
	    (midi->pig status (aref bytes 0)(aref bytes 1))))

	(defun sysex (bytes)
	  "(pig:sysex (data ....))
Sends sysex message to Pigiron operator named by named by pig:*midi-op*
Do not include either SYSEX of END-OF-SYSEX status bytes."
	  (apply #'midi->pig (append '(#xF0) (->list bytes) '(#xF7)))))
