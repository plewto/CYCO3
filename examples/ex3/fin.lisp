;;;; CYCO examples ex3 section fin
;;;; 
;;;; Restores MIDI controllers.

(section reset :bars 1)

(controllers volume-reset (list flute-1 flute-2 oboe clarinet
				question violin-1 violin-2 viola
				cello ensemble-treble ensemble-bass)
	     :events '((:cc (1 1 1) volume 127)))

(controllers portamento-reset question
	     :events '((:cc (1 1 1) portamento 0)))

(->midi reset)
