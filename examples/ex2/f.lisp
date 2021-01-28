;;;; CYCO example ex2 section f  -- fade out
;;;;
;;;; Section F is a clone of section E but adds volume fades to all parts.
;;;; Two fades are defined with slightly different rates.   The piano,
;;;; vibes and percussion fade out earlier then the synth and guitar.
;;;;

(param f (clone e :new-name "F"))

(controllers f-fade-1 (list piano vibes gm-snare)
	     :bars 8
	     :events '((:time (4 1 1) (7 3 1) s :ctrl volume :value 127 0 :ramp)))

(controllers f-fade-2 (list synth guitar)
	     :bars 8
	     :events '((:time (5 1 1)(8 4 1) s :ctrl volume :value 127 0 :ramp)))

(->midi f)

	     
