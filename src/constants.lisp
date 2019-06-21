;;;; CYCO
;;;; 


(constant +CYCO-VERSION+  (list 3 0 0))

(constant +BANNER+
	  (let ((a0 (format nil "  ___  _  _  ___  _____ ~%"))
		(a1 (format nil " / __)( \\/ )/ __)(  _  )~%"))
		(a2 (format nil "( (__  \\  /( (__  )(_)( ~%"))
		(a3 (format nil " \\___) (__) \\___)(_____)  3~%"))
		(a4 (format nil "~%")))
	    (concatenate 'string a0 a1 a2 a3 a4)))


(constant +TICKS-PER-BEAT+ 480)

;;; MIDI command bytes
;;;
(constant +ACTIVE-SENSING+ #xFE)
(constant +CHANNEL-PRESSURE+ #xD0)
(constant +CONTINUE+ #xFB)
(constant +CONTROL-CHANGE+ #xB0)
(constant +CLOCK+ #xF8)
(constant +END-EXCLUSIVE+ #xF7)
(constant +TIME-CODE+ #xF1)
(constant +NOTE-OFF+ #x80)
(constant +NOTE-ON+ #x90)
(constant +PITCH-BEND+ #xE0)
(constant +POLY-PRESSURE+ #xA0)
(constant +PROGRAM-CHANGE+ #xC0)
(constant +RESET+ #xFF)
(constant +SONG-POSITION-POINTER+ #xF2)
(constant +SONG-SELECT+ #xF3)
(constant +START+ #xFA)
(constant +SYSTEM-EXCLUSIVE+ #xF0)
(constant +TUNE-REQUEST+ #xF6)

;;; MIDI Meta types
;;;
(constant +META+ #xFF)
(constant +SEQUENCE-NUMBER+ #x00)
(constant +TEXT-MESSAGE+ #x01)
(constant +COPYRIGHT+ #x02)
(constant +TRACK-NAME+ #x03)
(constant +INSTRUMENT-NAME+ #x04)
(constant +LYRIC-TEXT+ #x05)
(constant +MARKER-TEXT+ #x06)
(constant +CUE-POINT+ #x07)
(constant +CHANNEL-PREFIX+ #x20)
(constant +END-OF-TRACK+ #x2F)
(constant +TEMPO-CHANGE+ #x51)
(constant +SMPTE-OFFSET+ #x54)
(constant +TIME-SIGNATURE+ #x58)
(constant +KEY-SIGNATURE+ #x59)
(constant +SEQ-SPECIFIC-EVENT+ #x7F)

;;; Map MIDI status and meta type to mnemonic string.
;;;
(constant +MNEMONICS+
	  (let ((table (make-hash-table :size 33)))
	    (setf (gethash +ACTIVE-SENSING+ table) "ASENS ")
	    (setf (gethash +CHANNEL-PRESSURE+ table) "CPRSS ")
	    (setf (gethash +CONTINUE+ table) "CONT  ")
	    (setf (gethash +CONTROL-CHANGE+ table) "CC    ")
	    (setf (gethash +CLOCK+ table) "CLOCK ")
	    (setf (gethash +END-EXCLUSIVE+ table) "EOX   ")
	    (setf (gethash +TIME-CODE+ table) "TCODE ")
	    (setf (gethash +NOTE-OFF+ table) "OFF   ")
	    (setf (gethash +NOTE-ON+ table) "ON    ")
	    (setf (gethash +PITCH-BEND+ table) "PBEND ")
	    (setf (gethash +POLY-PRESSURE+ table) "PPRSS ")
	    (setf (gethash +PROGRAM-CHANGE+ table) "PROG  ")
	    (setf (gethash +SONG-POSITION-POINTER+ table) "SPP   ")
	    (setf (gethash +SONG-SELECT+ table) "SS    ")
	    (setf (gethash +START+ table) "START ")
	    (setf (gethash +SYSTEM-EXCLUSIVE+ table) "SYSEX ")
	    (setf (gethash +TUNE-REQUEST+ table) "TUNE  ")
	    (setf (gethash +META+ table) "META  ")
	    (setf (gethash +SEQUENCE-NUMBER+ table) "SEQNUM   ")
	    (setf (gethash +TEXT-MESSAGE+ table) "TEXT     ")
	    (setf (gethash +COPYRIGHT+ table) "COPYRGHT ")
	    (setf (gethash +TRACK-NAME+ table) "TRK-NAME ")
	    (setf (gethash +INSTRUMENT-NAME+ table) "INS-NAME ")
	    (setf (gethash +LYRIC-TEXT+ table) "LYRIC    ")
	    (setf (gethash +MARKER-TEXT+ table) "MARKER   ")
	    (setf (gethash +CUE-POINT+ table) "CUEPOINT ")
	    (setf (gethash +CHANNEL-PREFIX+ table) "CHPREFIX ")
	    (setf (gethash +END-OF-TRACK+ table) "EOT      ")
	    (setf (gethash +TEMPO-CHANGE+ table) "TEMPO    ")
	    (setf (gethash +SMPTE-OFFSET+ table) "SMPTE    ")
	    (setf (gethash +TIME-SIGNATURE+ table) "TIME-SIG ")
	    (setf (gethash +KEY-SIGNATURE+ table) "KEY-SIG  ")
	    (setf (gethash +SEQ-SPECIFIC-EVENT+ table) "SSE      ")
	    table))

(constant +REST+ -1)
(constant +EQ12+ (expt 2.0d0 1/12))
(constant +CENT+ (expt 2.0d0 1/1200))



