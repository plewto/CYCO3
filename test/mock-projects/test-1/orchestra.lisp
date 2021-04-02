;;;; test-1 orchestra
;;;;

(prune-orchestra)

(general-midi-instrument piano :channel 1)
(general-midi-instrument bass :channel 2)
(general-midi-instrument organ :channel 3)
(general-midi-metronome :program 'woodblock :channel 16)


(pass? "Instruments bound"
       (and (boundp 'piano)(instrument-p piano)
	    (boundp 'bass)(instrument-p bass)
	    (boundp 'organ)(instrument-p organ)))
	    
(pass? "Instrument channels"
       (and (eq (channel-index piano) 0)
	    (eq (channel-index bass) 1)
	    (eq (channel-index organ) 2)
	    (eq (channel-index *metronome*) 15)))

(pass? "Orchestra tree structure"
       (let ((flag t))
	 (dolist (instrument (list piano bass organ *metronome*))
	   (setf flag (and flag (eq (parent instrument) +root-instrument+))))
	 flag))

