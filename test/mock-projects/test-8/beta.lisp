;;;; CYCO mock project 8, beta
;;;;
;;;; woodwinds only, channels 3 & 4.  2-bars
;;;;

(section beta :bars 2)

(simple-part a-sax sax
	     :events '((:time (1 1 1) :key c3)))

(simple-part a-oboe oboe
	     :events '((:time (2 1 1) :key c6)))
