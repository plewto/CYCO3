;;;; CYCO mock project test 8, alpha.
;;;;
;;;; Keys only, channels 1 & 2.
;;;;


(section alpha :bars 4)

(simple-part a-piano piano
	     :events '((:time (1 1 1) :key c3)))

(simple-part a-organ organ
	     :events '((:time (4 1 1) :key c6)))
