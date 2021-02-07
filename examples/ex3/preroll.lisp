;;;; CYCO examples ex3 preroll section
;;;;

(preroll
 :bars 1
 :instruments (list flute-1 flute-2 oboe clarinet
		    question violin-1 viola cello
		    ensemble-treble ensemble-bass
		    *metronome*))

(controllers initialize-volume (list flute-1 flute-2 oboe clarinet
				     question violin-1 violin-2 viola
				     cello ensemble-treble ensemble-bass)
	     :events '((:cc (1 1 1) volume 127)))

(controllers initialize-question-controllers question
	     :events '((:cc (1 1 1) :portamento-time 4)
		       (:cc (1 1 1) :portamento 0)))


(->midi preroll)
