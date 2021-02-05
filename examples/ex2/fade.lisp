;;;; CYCO example ex2 section f  -- fade out
;;;;
;;;; Section F is a clone of section E but adds volume fades to all parts.
;;;; Two fades are defined with slightly different rates.   The piano,
;;;; vibes and percussion fade out earlier then the synth and guitar.
;;;;

(param fade (clone e :new-name "Fade"))

(controllers fade-1 (list piano gm-snare guitar)
	     :bars 8
	     :events '((:time (4 1 1) (6 3 1) s :ctrl volume :value 127 0 :ramp)))

(controllers fade-2 (list synth vibes)
	     :bars 8
	     :events '((:time (5 1 1)(8 4 1) s :ctrl volume :value 127 0 :ramp)))

(->midi fade :filename "fade")

	     
