;;;; CYCO examples ex3 section A
;;;;

(section intro :bars 12)

(strummer intro-violin-1 violin-1
	  :events '((:time ( 1 1) :key g6  :dur 3*w)
		    (:time ( 4 1) :key fs6 :dur w. )
		    (:time ( 5 3) :key E6  :dur w  )
		    (:time ( 6 3) :key D6  :dur h  )
		    (:time ( 7 1) :key c6  :dur w+w+h.)
		    (:time ( 9 4) :key D6  :dur h)
		    (:time (10 2) :key C6  :dur w+w+h.)))

(strummer intro-violin-2 violin-2
	  :events '((:time ( 1 1) :key d5  :dur w+w+w+w.)
		    (:time ( 5 3) :key G4  :dur w.)
		    (:time ( 7 1) :key d5  :dur h)
		    (:time ( 7 3) :key e5  :dur w+w+w+w+w.)))
		      
(strummer intro-viola viola
	  :events '((:time ( 1 1) :key b3  :dur w+w+w+w.)
		    (:time ( 5 1) :key g4  :dur h+h.)
		    (:time ( 6 4) :key f4  :dur q)
		    (:time ( 7 1) :key e4  :dur w)
		    (:time ( 8 1) :key g4  :dur w+w+q)
		    (:time (10 1) :key a4  :dur w)
		    (:time (11 2) :key a3  :dur q)
		    (:time (11 3) :key b3)
		    (:time (11 4) :key c4)
		    (:time (12 1) :key d4)
		    (:time (12 2) :key e4)
		    (:time (12 3) :key d4)
		    (:time (12 4) :key c4)))

(strummer intro-cello cello
	  :events '((:time ( 1 1) :key g2  :dur 3*w)
		    (:time ( 4 1) :key b2  :dur w.)
		    (:time ( 5 3) :key b2)
		    (:time ( 7 1) :key c3  :dur h)
		    (:time ( 7 3) :key g2)
		    (:time ( 8 1) :key c2  :dur w.)
		    (:time ( 9 3) :key c3  :dur q)
		    (:time ( 9 4) :key b2  :dur h)
		    (:time (10 2) :key a2  :dur w+w+h.)))

(strummer intro-ensemble-high ensemble-high
	  :events '(;; violin-1
		     (:time ( 1 1) :key g6  :dur 3*w)
		     (:time ( 4 1) :key fs6 :dur w. )
		     (:time ( 5 3) :key E6  :dur w  )
		     (:time ( 6 3) :key D6  :dur h  )
		     (:time ( 7 1) :key c6  :dur w+w+h.)
		     (:time ( 9 4) :key D6  :dur h)
		     (:time (10 2) :key C6  :dur w+w+h.)
			    
		     ;; violin-2
		     (:time ( 1 1) :key d5  :dur w+w+w+w.)
		     (:time ( 5 3) :key G4  :dur w.)
		     (:time ( 7 1) :key d5  :dur h)
		     (:time ( 7 3) :key e5  :dur w+w+w+w+w.)))

(strummer intro-ensemble-bass ensemble-bass
	  :events '(;; viola
		     (:time ( 1 1) :key b3  :dur w+w+w+w.)
		     (:time ( 5 1) :key g4  :dur h+h.)
		     (:time ( 6 4) :key f4  :dur q)
		     (:time ( 7 1) :key e4  :dur w)
		     (:time ( 8 1) :key g4  :dur w+w+q)
		     (:time (10 1) :key a4  :dur w)
		     (:time (11 2) :key a3  :dur q)
		     (:time (11 3) :key b3)
		     (:time (11 4) :key c4)
		     (:time (12 1) :key d4)
		     (:time (12 2) :key e4)
		     (:time (12 3) :key d4)
		     (:time (12 4) :key c4)
		     
		     ;; cello
		     (:time ( 1 1) :key g2  :dur 3*w)
		     (:time ( 4 1) :key b2  :dur w.)
		     (:time ( 5 3) :key b2)
		     (:time ( 7 1) :key c3  :dur h)
		     (:time ( 7 3) :key g2)
		     (:time ( 8 1) :key c2  :dur w.)
		     (:time ( 9 3) :key c3  :dur q)
		     (:time ( 9 4) :key b2  :dur h)
		     (:time (10 2) :key a2  :dur w+w+h.)))

		    

(->midi intro)
			     
