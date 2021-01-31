;;;; CYCO example ex3 section q2
;;;;

(section q2 :bars 16)

(strummer q2-violin-1 violin-1
	  :events '((:time ( 1 1) :key g6  :dur 5*w)
		    (:time ( 6 1) :key fs6 :dur w+w.)
		    (:time ( 8 3) :key e6  :dur 4*w)
		    (:time (13 3) :key d6  :dur w)
		    (:time (14 3) :key c6  :dur 2*w)
		    (:time (16 3) :key b5  :dur h)))

(strummer q2-violin-2 violin-2
	  :events '((:time ( 1 1) :key d5  :dur 15*h)
		    (:time ( 8 3) :key g5  :dur 2*w)
		    (:time (11 3) :key c5  :dur 11*h)))

(strummer q2-viola viola
	  :events '((:time ( 1 1) :key b3  :dur w+w+w+h.)
		    (:time ( 4 4) :key c4  :dur h)
		    (:time ( 5 2) :key d4  :dur w+w+h+h.)
		    (:time ( 8 3) :key e4  :dur w.)
		    (:time (11 1) :key f4  :dur h)
		    (:time (11 3) :key g4  :dur w+w.)
		    (:time (14 1) :key a4  :dur 3*w)))

(strummer q2-cello cello
	  :events '((:time ( 1 1) :key b2  :dur q)
		    (:time ( 1 2) :key c3)
		    (:time ( 1 3) :key b2)
		    (:time ( 1 4) :key a2)
		    (:time ( 2 1) :key g2  :dur w+w+h.)
		    (:time ( 4 4) :key a2  :dur h)
		    (:time ( 5 2) :key b2  :dur w+w+h+h.)
		    (:time ( 8 3) :key c3  :dur w.)
		    (:time (11 1) :key d3  :dur h)
		    (:time (11 3) :key e3  :dur w+w.)
		    (:time (14 1) :key f3  :dur 3*w)))
		    
(strummer q2-ensemble-high ensemble-high
	  :events '((:time ( 1 1) :key g6  :dur 5*w)  ;; violin-1
		    (:time ( 6 1) :key fs6 :dur w+w.)
		    (:time ( 8 3) :key e6  :dur 4*w)
		    (:time (13 3) :key d6  :dur w)
		    (:time (14 3) :key c6  :dur 2*w)
		    (:time (16 3) :key b5  :dur h)

		    (:time ( 1 1) :key d5  :dur 15*h)  ;; violin-2
		    (:time ( 8 3) :key g5  :dur 2*w)
		    (:time (11 3) :key c5  :dur 11*h)))

(strummer q1-ensemble-bass ensemble-bass
	  :events '((:time ( 1 1) :key b3  :dur w+w+w+h.) ;; viola
		    (:time ( 4 4) :key c4  :dur h)
		    (:time ( 5 2) :key d4  :dur w+w+h+h.)
		    (:time ( 8 3) :key e4  :dur w.)
		    (:time (11 1) :key f4  :dur h)
		    (:time (11 3) :key g4  :dur w+w.)
		    (:time (14 1) :key a4  :dur 3*w)

		    (:time ( 1 1) :key b2  :dur q)  ;; cello
		    (:time ( 1 2) :key c3)
		    (:time ( 1 3) :key b2)
		    (:time ( 1 4) :key a2)
		    (:time ( 2 1) :key g2  :dur w+w+h.)
		    (:time ( 4 4) :key a2  :dur h)
		    (:time ( 5 2) :key b2  :dur w+w+h+h.)
		    (:time ( 8 3) :key c3  :dur w.)
		    (:time (11 1) :key d3  :dur h)
		    (:time (11 3) :key e3  :dur w+w.)
		    (:time (14 1) :key f3  :dur 3*w)))

(strummer q2-flute-1 flute-1
	  :events '((:time ( 1 t5 1) :key df5 :dur ht+h.   :amp p )
		    (:time ( 2 4  1) :key c5  :dur h              )
		    (:time ( 3 1  1) :key b4  :dur h.      :amp pp)
		    (:time ( 3 4  1) :key bf4 :dur q+s            )
		    (:time ( 3 4  3) :key a4                      )
		    (:time ( 4 1  1) :key af4 :dur q              )

		    (:time ( 9 2  1) :key a4  :dur w       :amp mp )
		    (:time (10 2  1) :key af4 :dur q.              )
		    (:time (10 3  4) :key g3  :dur e       :amp mp+)
		    (:time (10 4  1) :key af4 :dur q       :amp mf-)
		    (:time (11 1  1) :key f4  :dur q       :amp mf+)
		    (:time (11 2  1) :key f4  :dur h       :amp mp )))

(strummer q2-flute-2 flute-2
	  :events '((:time ( 1 t5 1) :key f4  :dur ht+q     :amp p )
		    (:time ( 2 1  1) :key g4  :dur 2*w             )

		    (:time ( 9 2  1) :key b4  :dur h.       :amp mp)
		    (:time (10 1  1) :key c5  :dur w+q             )
		    (:time (11 1  1) :key c5  :dur h               )))


(strummer q2-oboe oboe
	  :events '((:time ( 1 t5 1) :key c4  :dur ht+h.  :amp p )
		    (:time ( 2 4  1) :key b3  :dur h             )
		    (:time ( 3 1  1) :key c4  :dur w      :amp pp)

		    (:time ( 9 3  1) :key c4  :dur q      :amp mp)
		    (:time ( 9 4  1) :key g4                     )
		    (:time (10 1  1) :key f4  :dur h.+q.         )
		    (:time (11 3  1) :key f4  :dur e             )))

(strummer q2-clarinet clarinet
	 :events '((:time ( 1 t5 1) :key fs3  :dur ht+q  :amp p  )
		   (:time ( 2 1  1) :key gs3  :dur w.            )
		   (:time ( 3 4  1) :key a3   :dur e.    :amp pp )
		   (:time ( 3 4  3) :key as3                     )
		   (:time ( 4 1  1) :key b3   :dur q             )

		   (:time ( 9 1  1) :key gs4  :dur w     :amp mp )
		   (:time (10 2  1) :key a4   :dur q             )
		   (:time (10 3  1) :key b4   :dur q.            )
		   (:time (10 4  3) :key d5   :dur e             )
		   (:time (11 1  1) :key cs5  :dur h             )
		   (:time (11 3  1) :key cs5  :dur e             )))
		   



(param q2-question-1 (the-question 'q2-question-1 '(6 2 1)))
(param q2-question-2 (the-question 'q2-question-2 '(15 t5 1) :end-note 'b4))

(->midi q2)
