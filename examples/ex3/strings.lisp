;;;; CYCO ex3 strings
;;;;

;; violin-1
;;
(param v1-event-list '((:time ( 1 1) :key g6  :dur 3*w)
		       (:time ( 4 1) :key fs6 :dur w.)
		       (:time ( 5 3) :key e6  :dur w)
		       (:time ( 6 3) :key d6  :dur h)
		       (:time ( 7 1) :key c6  :dur w+w+h.)
		       (:time ( 9 4) :key d6  :dur h)
		       (:time (10 2) :key c6  :dur w+w+h.)
		       (:time (13 1) :key g6  :dur 4*w)
		       (:time (17 1) :key fs6 :dur w.)
		       (:time (18 3) :key e6  :dur w)
		       (:time (19 3) :key d6  :dur h)
		       (:time (20 1) :key c6  :dur w+w+h.)
		       (:time (22 4) :key d6  :dur h)
		       (:time (23 2) :key c6  :dur w+w+h.)
		       (:time (26 1) :key g6  :dur 5*w)
		       (:time (31 1) :key fs6 :dur w+w+h)
		       (:time (33 3) :key e6  :dur 4*w)
		       (:time (37 3) :key d6  :dur w)
		       (:time (38 3) :key c6  :dur 2*w)
		       (:time (40 3) :key b5  :dur w.)
		       (:time (42 1) :key a5  :dur w)
		       (:time (43 1) :key g5  :dur w.)
		       (:time (44 3) :key f5  :dur h)
		       (:time (45 1) :key e5  :dur w)
		       (:time (46 1) :key g5  :dur h)
		       (:time (46 3) :key a5  :dur h)
		       (:time (47 1) :key b5  :dur w)
		       (:time (48 1) :key a5  :dur h.)
		       (:time (48 4) :key g5  :dur w+q)
		       (:time (50 1) :key d6  :dur h)
		       (:time (50 3) :key c6  :dur w..)
		       (:time (52 2) :key f6  :dur q)
		       (:time (52 3) :key e6  :dur q)
		       (:time (52 4) :key d6  :dur w+q)
		       (:time (54 1) :key e6  :dur h)
		       (:time (54 3) :key fs6 :dur w)
		       (:time (55 3) :key g6  :dur 13*h)))

;; violin-2
;;
(param v2-event-list '((:time ( 1 1) :key d5 :dur 9*h)
		       (:time ( 5 3) :key g4 :dur w.)
		       (:time ( 7 1) :key d5 :dur h)
		       (:time ( 7 3) :key e5 :dur 11*h)
		       (:time (13 1) :key d5 :dur 11*h)
		       (:time (18 3) :key g4 :dur w.)
		       (:time (20 1) :key d5 :dur h)
		       (:time (20 3) :key e5 :dur 11*h)
		       (:time (26 1) :key d5 :dur 15*h)
		       (:time (33 3) :key g5 :dur 2*w)
		       (:time (35 3) :key c5 :dur 11*h)
		       (:time (41 1) :key e5 :dur 7*h)
		       (:time (44 3) :key c5 :dur w.)
		       (:time (46 1) :key e5 :dur h)
		       (:time (46 3) :key f5 :dur h)
		       (:time (47 1) :key e5 :dur w)
		       (:time (48 1) :key f5 :dur h)
		       (:time (48 3) :key c5 :dur h)
		       (:time (49 1) :key d5 :dur w)
		       (:time (50 1) :key c5 :dur 2*w)
		       (:time (52 1) :key d5 :dur 2*w)
		       (:time (54 1) :key c5 :dur h)
		       (:time (54 3) :key a4 :dur h)
		       (:time (55 1) :key e5 :dur h)
		       (:time (54 3) :key d5 :dur h)
		       (:time (56 1) :key c5 :dur h)
		       (:time (56 3) :key d5 :dur h)
		       (:time (57 1) :key e5 :dur h)
		       (:time (57 3) :key d5 :dur 9*h)))

(param viola-event-list '((:time ( 1 1) :key b3 :dur 10*h)
			  (:time ( 5 3) :key g4 :dur h+h.)
			  (:time ( 6 4) :key f4 :dur q)
			  (:time ( 7 1) :key e4 :dur w)
			  (:time ( 8 1) :key g4 :dur w+w+q)
			  (:time (10 1) :key g4 :dur q)
			  (:time (10 2) :key a4 :dur w)
			  (:time (11 2) :key a3 :dur q)
			  (:time (11 3) :key b3 :dur q)
			  (:time (11 4) :key c4 :dur q)
			  (:time (12 1) :key d4 :dur q)
			  (:time (12 2) :key e4 :dur q)
			  (:time (12 3) :key d4 :dur q)
			  (:time (12 4) :key c4 :dur q)
			  (:time (13 1) :key b3 :dur 11*h)
			  (:time (20 1) :key e4 :dur w)
			  (:time (21 1) :key g4 :dur w+w+q)
			  (:time (23 2) :key a4 :dur w)
			  (:time (24 2) :key a3 :dur q)
			  (:time (24 3) :key b3 :dur q)
			  (:time (24 4) :key c4 :dur q)
			  (:time (25 1) :key d4 :dur q)
			  (:time (25 2) :key e4 :dur q)
			  (:time (25 3) :key d4 :dur q)
			  (:time (25 4) :key c4 :dur q)
			  (:time (26 1) :key b3 :dur w+w+w+h.)
			  (:time (29 4) :key c4 :dur h)
			  (:time (30 2) :key d4 :dur w+w+w+q)
			  (:time (33 3) :key e4 :dur w.)
			  (:time (35 1) :key f4 :dur h)
			  (:time (35 3) :key g4 :dur w+w.)
			  (:time (38 1) :key a4 :dur 3*w)
			  (:time (41 1) :key b4 :dur w)
			  (:time (42 1) :key c5 :dur w)
			  (:time (43 1) :key b4 :dur w.)
			  (:time (44 3) :key g4 :dur w.)
			  (:time (46 1) :key b4 :dur h)
			  (:time (46 3) :key c5 :dur h)
			  (:time (47 1) :key b4 :dur w)
			  (:time (48 1) :key c5 :dur h)
			  (:time (48 3) :key g4 :dur w)
			  (:time (49 3) :key f4 :dur h)
			  (:time (50 1) :key e4 :dur w)
			  (:time (51 1) :key e4 :dur h)
			  (:time (51 3) :key d4 :dur q)
			  (:time (51 4) :key c4 :dur h)
			  (:time (52 2) :key b3 :dur 2*w)
			  (:time (54 2) :key c4 :dur w+q)
			  (:time (55 3) :key b3 :dur h)
			  (:time (56 1) :key a3 :dur h)
			  (:time (56 3) :key b3 :dur h)
			  (:time (57 1) :key c4 :dur h)
			  (:time (57 3) :key b3 :dur 9*h)))

(param cello-event-list '((:time ( 1 1) :key g2  :dur 3*w)
			  (:time ( 4 1) :key b2  :dur w.)
			  (:time ( 5 3) :key b2  :dur w.)
			  (:time ( 6 1) :key c3  :dur h)
			  (:time ( 6 3) :key g2  :dur h)
			  (:time ( 7 1) :key c2  :dur w.)
			  (:time ( 8 3) :key c3  :dur q)
			  (:time ( 8 4) :key b2  :dur h)
			  (:time ( 9 2) :key a2  :dur w+w+h.)
			  (:time (13 1) :key b2  :dur q)
			  (:time (13 2) :key c3  :dur q)
			  (:time (13 3) :key b2  :dur q)
			  (:time (13 4) :key a2  :dur q)
			  (:time (14 1) :key g2  :dur 3*w)
			  (:time (17 1) :key b2  :dur w.)
			  (:time (18 3) :key b2  :dur w.)
			  (:time (20 1) :key c3  :dur h)
			  (:time (20 3) :key g2  :dur h)
			  (:time (21 1) :key c2  :dur w.)
			  (:time (22 3) :key c3  :dur q)
			  (:time (22 4) :key b2  :dur h)
			  (:time (23 2) :key a2  :dur w+w+h.)
			  (:time (26 1) :key b2  :dur q)
			  (:time (26 2) :key c3  :dur q)
			  (:time (26 3) :key b2  :dur q)
			  (:time (26 4) :key a2  :dur q)
			  (:time (27 1) :key g2  :dur w+w+h.)
			  (:time (29 4) :key a2  :dur h)
			  (:time (30 2) :key b2  :dur w+w+w+q)
			  (:time (33 3) :key c3  :dur w.)
			  (:time (35 1) :key d3  :dur h)
			  (:time (35 3) :key e3  :dur w+w.)
			  (:time (38 1) :key f3  :dur 3*w)
			  (:time (41 1) :key g3  :dur w)
			  (:time (42 1) :key a3  :dur w)
			  (:time (43 1) :key b3  :dur w.)
			  (:time (44 3) :key c4  :dur w.)
			  (:time (46 1) :key b3  :dur h)
			  (:time (46 3) :key a3  :dur h)
			  (:time (47 1) :key g3  :dur w)
			  (:time (48 1) :key f3  :dur h)
			  (:time (48 3) :key e3  :dur h)
			  (:time (49 1) :key b2  :dur w)
			  (:time (50 1) :key c3  :dur w)
			  (:time (51 1) :key a2  :dur w)
			  (:time (52 1) :key g2  :dur 10*w)))


;; A shuffle function adds slight time randomization.
;;
(labels ((coin (&optional (p 0.5)) 
	       (< (random 1.0) p))
	 (randomizer (cue)
		     (declare (ignore cue))
		     (if (coin)
			 (random 0.1)
		       0.0)))
  (strummer violin-1-part violin-1 :events v1-event-list :shuffle #'randomizer)
  (strummer violin-2-part violin-2 :events v2-event-list :shuffle #'randomizer)
  (strummer viola-part viola :events viola-event-list :shuffle #'randomizer)
  (strummer cello-part cello :events cello-event-list :shuffle #'randomizer)
  (strummer high-strings ensemble-treble
	    :events (append v1-event-list v2-event-list)
	    :shuffle #'randomizer)
  (strummer low-strings ensemble-bass
	    :events (append viola-event-list cello-event-list)
	    :shuffle #'randomizer))


;; Fade strings starting at bar 59.
;;
(controllers strings-volume (list violin-1 violin-2 viola cello
				  ensemble-treble ensemble-bass)
	     :events '((:time (59 1 1)(61 4 4) t :value 127 0 :ctrl volume :ramp)))
		      
