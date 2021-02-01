;;;; CYCO mock project test 8, alpha.
;;;;
;;;; Keys only, channels 1 & 2.
;;;;


(section alpha :bars 4)

(qball a-piano piano
       :cue '((1 1 1))
       :key 'd3)

(strummer a-organ organ
	  :events '((:time (4 1 1) :key c6)))
