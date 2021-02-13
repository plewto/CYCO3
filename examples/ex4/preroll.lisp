;;;; CYCO examples ex4 preroll.lisp
;;;;

(preroll
 :bars 1
 :instruments (list piano gm-percussion *metronome*))

(controllers initialize-controllers (list piano gm-percussion *metronome*)
	     :events '((:cc (1 1 1) volume 127)
		       (:cc (1 1 1) portamento 0)))

(bender initialize-pitch-bend (list piano gm-percussion *metronome*)
	:events '((:bend (1 1 1) 0.0)))

(->midi preroll)
