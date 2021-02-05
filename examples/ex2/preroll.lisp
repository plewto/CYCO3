;;;; CYCO examples ex2 preroll.lisp
;;;;
;;;; Defines the initial 'preroll' to send initial program-change events
;;;; and provide a count-in metronome.
;;;;

(preroll
 :bars 1
 :instruments (list piano vibes synth guitar *metronome*))

(controllers initialize-controllers (list piano vibes synth guitar *metronome*)
	     :events '((:cc (1 1 1) volume 127)
		       (:cc (1 1 1) portamento 0)))

;; Save section to MIDI file.
;;
(->midi preroll)
