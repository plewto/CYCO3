;;;; CYCO example ex1 preroll.lisp
;;;;
;;;; A preroll section is used for initial program changes and as a
;;;; count-in metronome.
;;;;


;;; Create the preroll section of 1 bar length.  The project's 
;;; default time-signature is overridden for a 4/4 time.
;;;
;;; Program change events are generated for each listed instrument
;;;
;;; The new section is bound to the symbol PREROLL.
;;;

(param foo (preroll
	    :bars 1
	    :beats 4
	    :instruments (list piano flute vibes *metronome*)))

;;; Write the preroll section to a MIDI file.
;;;

(->midi preroll)
