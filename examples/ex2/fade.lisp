;;;; CYCO example ex2 section f  -- fade out
;;;;
;;;; Section F is a clone of section E with volume events
;;;; for a final fade out.  Three controllers parts are used
;;;; to fade instruments at different rates.
;;;;

(param fade (clone e :new-name "Fade"))
(bulk-rename-parts fade 1 "fade")

(controllers fade-1 (list bass gm-snare guitar)
	     :bars 8
	     :events '((:time (1 1 1) (6 3 1) s :ctrl volume :value 127 0 :ramp)))

(controllers fade-2 piano
	     :bars 8
	     :events '((:time (2 3 1)(7 3 1) s :ctrl volume :value 127 0 :ramp)))

(controllers fade-3 (list synth vibes)
	     :bars 8
	     :events '((:time (4 1 1)(8 4 1) s :ctrl volume :value 127 0 :ramp)))

(->midi fade :filename "fade")

	     
