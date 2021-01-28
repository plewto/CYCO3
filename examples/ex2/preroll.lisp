;;;; CYCO examples ex2 preroll.lisp
;;;;
;;;; Defines the initial 'preroll' to send initial program-change events
;;;; and provide a count-in metronome.
;;;;

(preroll
 :bars 1
 :instruments (list piano vibes synth guitar *metronome*))

;; Save section to MIDI file.
;;
(->midi preroll)
