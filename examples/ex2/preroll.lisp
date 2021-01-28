;;;; CYCO examples ex2 preroll.lisp
;;;;

(preroll
 :bars 1
 :instruments (list piano vibes synth guitar *metronome*))

(controllers preroll-volume (list piano vibes gm-snare synth guitar)
	     :bars 8
	     :events '((:cc (1 1 1) volume 127)))

(->midi preroll)
