;;;; CYCO examples ex2 preroll.lisp
;;;;
;;;; Defines the 'preroll' section to initialize MIDI programs
;;;; and controller values.  The preroll also provides a count-in 
;;;; metronome.
;;;;

(preroll
 :bars 1
 :instruments (list bass piano vibes synth guitar *metronome*))

(controllers initialize-controllers
	     (list bass piano vibes synth guitar gm-percussion *metronome*)
	     :events '((:cc (1 1 1) volume 127)
		       (:cc (1 1 1) portamento 0)))

;; Save section to MIDI file.
;;
(->midi preroll)
