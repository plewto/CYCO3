;;;; CYCO examples ex3 preroll section
;;;;

(preroll
 :bars 1
 :instruments (list flute-1 flute-2 oboe clarinet
		    question violin-1 viola cello
		    ensemble-treble ensemble-bass
		    *metronome*))
		    


(->midi preroll)
