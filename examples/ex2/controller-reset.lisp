;;;; CYCO example ex2 section volume-reset
;;;; The final section used solely to reset controller values.
;;;;

(fin controller-reset :bars 4)

(make-controllers 'controller-reset (list piano vibes gm-snare synth guitar)
		  :bars 4
		  :events '((:cc (2 1 1) volume 127)
			    (:cc (2 1 1) portamento 0)))

(->midi controller-reset)
