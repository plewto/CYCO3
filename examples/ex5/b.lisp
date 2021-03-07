;;;; CYCO Example ex5 b   Strumming chords
;;;;

(section b :bars 8)

(strummer b1 piano
	  :events '((:amp ff :chord (0 4 7) :dur q)  ;; major 

		    ;; Normally all chord notes sound together.
		    (:time (1 1 1) :key c5 :dur e)      

		    ;; :strum is used to stagger the timing and may either 
		    ;; be in absolute seconds...
		    (:time (1 2 1) :key d5 :strum 0.01)

		    ;; ... or as a metric-expression.
		    ;; Numeric strum times are not scaled by tempo,
		    ;; symbolic times are tempo-scaled.
		    (:time (1 3 1) :key d5 :strum x)

		    ;; Note order may be altered by the :direction keyword.
		    (:strum s)
		    (:time (2 1 1) :key c3 :direction down)   ;; forward
		    (:time (2 2 1) :key c4 :direction up)     ;; reverse
		    (:time (2 3 1) :key c5 :direction random) ;; scramble order
		    (:time (2 4 1) :key c6 :direction dice)   ;; select up or down at random

		    ;; A pattern of directions may be applied over the next 
		    ;; several chords.   The pattern cycles once it ends.
		    (:strum s :direction (down up))
		    (:time (3 1 1) :key d3 )   ;; down
		    (:time (3 2 1) :key d4 )   ;; up
		    (:time (3 3 1) :key d5 )   ;; down
		    (:time (3 4 1) :key d6 )   ;; up

		    ;; The strum rate may be accelerated by :strum*
		    ;; :strum* n     n == 1    --> no scale
		    ;; :strum* n     n > 1     --> slow down
		    ;; :strum* n     0 < n < 1 --> speed up
		    (:strum s :direction down :chord (0 3 7 12))
		    (:time (4 1 1) :key e4  :strum* 1)    ;; no acceleration
		    (:time (4 2 1) :key g5  :strum* 0.5)  ;; accelerate
		    (:time (4 3 1) :key a6  :strum* 1.5)  ;; decelerate

		    ;; Note amplitude may be scale across a chord.
		    ;; The first chord is played without amplitude scaling
		    ;;
		    (:strum s :direction down :chord (0 3 7 12) :strum* 1.0 :amp f :dur q)
		    (:time (5 1 1) :key c4  :amp* 1.0)  ;; no scale
		    (:time (5 3 1) :key d4  :amp* 0.8)  ;; later note have lower amp
		    (:time (6 1 1) :key e4  :amp* 1.2)  ;; later note have higher amp

		    ;; The end-times for strummed notes may either be staggered
		    ;; in the order they are played, or held until the last note
		    ;; ends.  This effect can be subtle, particuarly with 
		    ;; percussive instruments.
		    (:strum s :chord (0 3 7 12) :strum* 1.0 :amp ff :dur q)
		    (:time (7 1 1) :key d5 :end-together no)
		    (:time (8 1 1) :key d5 :end-together yes)))
(->midi b)
