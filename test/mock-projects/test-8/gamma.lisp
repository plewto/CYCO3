;;;; CYCO mock project test 8, gamma
;;;;
;;;; brass instruments only, channels 5 & 6.
;;;;

(section gamma :bars 4)

(simple-part a-trumpet trumpet
	     :events '((:time (1 1 1) :key c4)))

(simple-part a-trombone trombone
	     :events '((:time (1 1 1) :key c4)))
