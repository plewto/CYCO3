;;;; CYCO example ex3 section q1
;;;;

(section q1 :bars 13)

(flet ((key-list (&optional (end-note 'c5))
		 (append '(bf4 cs4 e4 ef5) (list end-note)))
       (duration-list (&optional (end-duration 'ht))
		      (append '(h+qt qt qt h+e)(list end-duration))))
  
  (defun the-question (name start-time &key (end-note 'c5)(end-duration 'q))
    (let ((time-sig (property *project* :current-section)))
      (make-qball name question
		  :amp 'mf
		  :shift (bar  time-sig start-time)
		  :cue '((1 1 1)(1 T5 1)(1 T6 1)(2 1 1)(2 3 3))
		  :key (key-list end-note)
		  :dur (duration-list end-duration))))) 

(strummer q1-violin-1 violin-1
	  :events '((:time ( 1 1) :key g6  :dur 4*w)
		    (:time ( 5 1) :key fs6 :dur w. )
		    (:time ( 6 3) :key E6  :dur w  )
		    (:time ( 7 3) :key D6  :dur h  )
		    (:time ( 8 1) :key c6  :dur w+w+h.)
		    (:time (10 4) :key D6  :dur h)
		    (:time (11 2) :key C6  :dur w+w+h.)))

(strummer q1-violin-2 violin-2
	  :events '((:time ( 1 1) :key d5  :dur w+w+w+w+w.)
		    (:time ( 6 3) :key g4  :dur w.)
		    (:time ( 8 1) :key d5  :dur h)
		    (:time ( 8 3) :key e5  :dur w+w+w+w+w.)))

(strummer q1-viola viola
	  :events '((:time ( 1 1) :key b3  :dur w+w+w+w+w.)
		    (:time ( 6 3) :key g4  :dur w+q)
		    (:time ( 7 4) :key f4  :dur q)
		    (:time ( 8 1) :key g4  :dur w+w+q)
		    (:time (11 2) :key a4  :dur w)
		    (:time (12 2) :key a3  :dur q)
		    (:time (12 3) :key b3)
		    (:time (12 4) :key c4)
		    (:time (13 1) :key d4)
		    (:time (13 2) :key e4)
		    (:time (13 3) :key d4)
		    (:time (13 4) :key c4)))

(strummer q1-cello cello
	  :events '((:time ( 1 1) :key b2 :dur q)
		    (:time ( 1 2) :key c3)
		    (:time ( 1 3) :key b2)
		    (:time ( 1 4) :key a2)
		    (:time ( 2 1) :key g2 :dur 3*w)
		    (:time ( 5 1) :key b2 :dur w.)
		    (:time ( 6 3) :key b2)
		    (:time ( 8 1) :key c3 :dur h)
		    (:time ( 8 3) :key g2)
		    (:time ( 9 1) :key c2 :dur w.)
		    (:time (10 3) :key c3 :dur q)
		    (:time (10 4) :key b2 :dur h)
		    (:time (11 2) :key a2 :dur w+w+h.))) 

(strummer q1-ensemble-high ensemble-high
	  :events '((:time ( 1 1) :key g6  :dur 4*w) ;; violin-1
		    (:time ( 5 1) :key fs6 :dur w. )
		    (:time ( 6 3) :key E6  :dur w  )
		    (:time ( 7 3) :key D6  :dur h  )
		    (:time ( 8 1) :key c6  :dur w+w+h.)
		    (:time (10 4) :key D6  :dur h)
		    (:time (11 2) :key C6  :dur w+w+h.)

		    (:time ( 1 1) :key d5  :dur w+w+w+w+w.) ;; violin-2
		    (:time ( 6 3) :key g4  :dur w.)
		    (:time ( 8 1) :key d5  :dur h)
		    (:time ( 8 3) :key e5  :dur w+w+w+w+w.)))

(strummer q1-ensemble-bass ensemble-bass
	  :events '((:time ( 1 1) :key b3  :dur w+w+w+w+w.) ;; viola
		    (:time ( 6 3) :key g4  :dur w+q)
		    (:time ( 7 4) :key f4  :dur q)
		    (:time ( 8 1) :key g4  :dur w+w+q)
		    (:time (11 2) :key a4  :dur w)
		    (:time (12 2) :key a3  :dur q)
		    (:time (12 3) :key b3)
		    (:time (12 4) :key c4)
		    (:time (13 1) :key d4)
		    (:time (13 2) :key e4)
		    (:time (13 3) :key d4)
		    (:time (13 4) :key c4)

		    (:time ( 1 1) :key b2 :dur q) ;; cello
		    (:time ( 1 2) :key c3)
		    (:time ( 1 3) :key b2)
		    (:time ( 1 4) :key a2)
		    (:time ( 2 1) :key g2 :dur 3*w)
		    (:time ( 5 1) :key b2 :dur w.)
		    (:time ( 6 3) :key b2)
		    (:time ( 8 1) :key c3 :dur h)
		    (:time ( 8 3) :key g2)
		    (:time ( 9 1) :key c2 :dur w.)
		    (:time (10 3) :key c3 :dur q)
		    (:time (10 4) :key b2 :dur h)
		    (:time (11 2) :key a2 :dur w+w+h.)))

(strummer q1-flute-1 flute-1
	  :events '((:time (8 T3 1) :key f4  :dur wt+ht+et  :amp pp)
		    (:time (9 3 T1) :key e4  :dur et        :amp pp-)
		    (:time (9 3 T5) :key ef4                :amp ppp+)
		    (:time (9 4 1 ) :key d5  :dur q+qt      :amp ppp)))

(strummer q1-flute-2 flute-2
	  :events '((:time (8 2 T3) :key fs4  :dur ht+q+w  :amp p)))

(strummer q1-oboe oboe
	  :events '((:time (8 4 T3) :key b4   :dur wt+ht  :amp pp)))

(strummer q1-clarinet clarinet
	  :events '((:time (8 3 T1) :key d4   :dur wt+et  :amp pp  )
		    (:time (9 3 T3) :key ds4  :dur et     :amp pp- )
		    (:time (9 3 t5) :key e4               :amp ppp+)
		    (:time (9 4 t1) :key f4   :dur 3*et   :amp ppp )))
		    

(param q1-question-1 (the-question 'q1-question-1 '(4 1 1)))
(param q1-question-2 (the-question 'q1-question-2 '(11 4 1) :end-note 'b4))


(->midi q1)


		
