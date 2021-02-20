;;;; CYCO examples ex4 preroll.lisp
;;;;

(let ((instruments (list piano bass guitar strings high-strings
			 clarinet gm-percussion *metronome*)))

  (preroll :bars 1 :instruments instruments)

  (controllers initialize-controllers instruments
	       :events '((:cc (1 1 1) volume 127)
			 (:cc (1 1 1) portamento 0)))

  (bender initialize-pitch-bend instruments
	  :events '((:bend (1 1 1) 0.0))))

(->midi preroll)
