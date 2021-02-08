;;;; CYCO 3 example ex3 "answers"
;;;;

;; Answer 1  Bar 20
;;
(strummer a1-flute-1 flute-1
	  :events '((:time (t 20 5) :key f5  :dur wt+ht+et :amp pp)
		    (:time (t 21 6) :key e5  :dur et )
		    (:time (t 21 7) :key ef5         )
		    (:time (t 21 8) :key d6  :dur ht )))

(strummer a1-flute-2 flute-2
	  :events '((:time (t 20 6) :key gs3  :dur 17*et :amp pp)))

(strummer a1-oboe oboe
	  :events '((:time (t 20 11) :key b3  :dur 6*qt :amp pp)))

(strummer a1-clarinet clarinet
	  :events '((:time (t 20 9) :key df4 :dur 9*et  :amp pp)
		    (:time (t 21 6) :key ds4 :dur et)
		    (:time (t 21 7) :key e4  :dur et)
		    (:time (t 21 8) :key f4  :dur 3*et)))

(controllers a1-flute-dynamics (list flute-1 oboe)
	     :events '((:time (20 t2 1)(22 1 1) t :value 127 16 :ctrl volume :ramp)
		       (:cc   (22 1 1) volume 127)))

;; Answer 2 Bar 26
;;
(strummer a2-flute-1 flute-1
	  :events '((:time (26 t5)  :key df6 :dur ht+h.  :amp pp )
		    (:time (27 4 1) :key c6  :dur h   )
		    (:time (28 2 1) :key b5  :dur h   )
		    (:time (28 4 1) :key bf5 :dur e   )
		    (:time (28 4 3) :key a5  :dur e   )
		    (:time (29 1 1) :key af5 :dur q   )))

(strummer a2-flute-2 flute-2
	  :events '((:time (26 t5)   :key f5  :dur ht+q  :amp pp+)
		    (:time (27 2  1) :key g5  :dur 2*w)))

(strummer a2-oboe oboe
	  :events '((:time (26 t5)  :key c5  :dur ht+h.  :amp pp+)
		    (:time (27 4 1) :key b4  :dur h)
		    (:time (28 2 1) :key c5  :dur w)))

(strummer a2-clarinet clarinet
	  :events '((:time (26 t5)  :key fs4 :dur ht+q  :amp pp+)
		    (:time (27 2 1) :key gs4 :dur 2*h.)
		    (:time (28 4 1) :key a4  :dur e   )
		    (:time (28 4 3) :key as4 :dur e   )
		    (:time (29 1 1) :key b4  :dur q   )))

;; Answer 3 Bar 34 
;;

(strummer a3-flute-1 flute-1
	  :events '((:time (34 1 1) :key a5  :dur w  :amp  p)
		    (:time (35 2 1) :key af5 :dur q.)
		    (:time (35 3 4) :key g5  :dur e )
		    (:time (35 4 1) :key gf5 :dur q )
		    (:time (36 1 1) :key f5  :dur q)
		    (:time (36 2 1) :key f5  :dur h )))

(strummer a3-flute-2 flute-2
	  :events '((:time (34 1 1) :key b4  :dur h.  :amp p)
		    (:time (35 1 1) :key c5  :dur w+q)
		    (:time (36 2 1) :key c5  :dur h)))

(strummer a3-oboe oboe
	  :events '((:time (34 3 1) :key c5  :dur q   :amp p)
		    (:time (34 4 1) :key g5  :dur q)
		    (:time (35 1 1) :key f5  :dur q)
		    (:time (35 2 1) :key e5  :dur h.+h)
		    (:time (36 3 1) :key e5  :dur e)))

(strummer a3-clarinet clarinet
	  :events '((:time (34 2 1) :key gs4 :dur w  :amp p)
		    (:time (35 2 1) :key a4  :dur q )
		    (:time (35 3 1) :key b4  :dur q.)
		    (:time (35 4 3) :key d5  :dur e )
		    (:time (36 1 1) :key cs5 :dur h)
		    (:time (36 3 1) :key cs5 :dur e)))
		    
(controllers a3-dynamics (list flute-1 flute-2 oboe clarinet)
	     :events '((:cc (34 1 1) volume 112)
		       (:time (35 3 3)(36 1 1) t :value 112 127 :ctrl volume :ramp)
		       (:time * (36 4 1) :value 127 32 :ramp)
		       (:cc (37 1 1) volume 127)))


;; Answer 4 Bar 41

(strummer a4-flute-1 flute-1
	  :events '((:time (41 2 1) :key c6  :dur h :amp pp)
		    (:time (41 4 1) :key fs5 :dur q)
		    (:time (42 1 1) :key af5 :dur q)
		    (:time (42 2 1) :key ef6 :dur h)
		    (:time (42 4 1) :key cs6 :dur q)
		    (:time (43 1 1) :key b5  :dur e)
		    (:time (43 1 3) :key bf5 :dur s)
		    (:time (43 1 4) :key a5  :dur s)
		    (:time (43 2 1) :key g6  :dur h)))

(strummer a4-flute-2 flute-2
	  :events '((:time (41 2 1) :key a5  :dur h  :amp pp)
    		    (:time (41 4 1) :key ds5 :dur q)
		    (:time (42 1 1) :key f5  :dur q)
		    (:time (42 2 1) :key c6  :dur h)
		    (:time (42 4 1) :key gs5 :dur q)
		    (:time (43 1 1) :key b5  :dur e)
		    (:time (43 1 3) :key bf5 :dur s)
		    (:time (43 1 4) :key a5  :dur s)
		    (:time (43 2 1) :key g6  :dur h)))

(strummer a4-oboe oboe
	  :events '((:time (41 2 1) :key b4  :dur h :amp p)
		    (:time (41 4 1) :key g4  :dur q)
		    (:time (42 1 1) :key fs4 :dur q)
		    (:time (42 2 1) :key e4  :dur e)
		    (:time (42 2 3) :key f4  :dur e)
		    (:time (42 3 1) :key e5  :dur e)
		    (:time (42 3 3) :key f5  :dur e)
		    (:time (42 4 1) :key d5  :dur q)
		    (:time (43 1 1) :key c5  :dur e)
		    (:time (43 1 3) :key b4  :dur s)
		    (:time (43 1 4) :key bf4 :dur s)
		    (:time (43 2 1) :key fs5 :dur h)))

(defchord '[gliss] '(0 0 1 2 2 3 4 5 6 6 7 8 8 9
		       10 10 11 12 12 12 12 12 12 12 12))
(strummer a4-clarinet clarinet
	  :events '((:time (41 2 1) :key gs4  :dur h  :amp p)
		    (:time (41 4 1) :key e4   :dur q)
		    (:time (42 1 1) :key d4   :dur q)
		    (:time (42 2 1) :key cs4  :dur e)
		    (:time (42 2 3) :key d4   :dur e)
		    (:time (42 3 1) :key b4   :dur e)
		    (:time (42 3 3) :key c5   :dur e)
		    (:time (42 4 1) :key a4   :dur q)
		    (:time (43 1 1) :key ds4  :chord [gliss] :dur t :strum tt :end-together nil :amp p-)
		    (:time (43 1 4) :key ds5  :chord [solo] :dur h+s :amp p)))

(controllers a4-dynamics (list flute-1 flute-2 oboe clarinet)
	     :events '((:cc (41 1 1) volume 100)
		       (:time (42 1 1) (41 1 1) t :value 100 127 :ctrl volume :ramp)
		       (:time * (43 2 1) t :value 100 127 :ramp)
		       (:cc (44 1) volume 127)))

;; Answer 5 Bar 47
;;

(strummer a5-flute-1 flute-1
	  :events '((:time (  47 4 1) :key c6  :dur e  :amp p)
		    (:time (  47 4 3) :key d5  :dur e)
		    (:time (  48 1 1) :key ef5 :dur e)
		    (:time (  48 1 3) :key d6  :dur e)
		    (:time (t 48 4)   :key f6  :dur et)
		    (:time (t 48 5)   :key e6  :dur et)
		    (:time (t 48 6)   :key f6  :dur et)
		    (:time (t 48 7)   :key e6  :dur et)
		    (:time (t 48 8)   :key f6  :dur et)
		    (:time (t 48 9)   :key e6  :dur et)
		    (:time (  48 4 1) :key ef6 :dur e)
		    (:time (  48 4 3) :key d6  :dur e)
		    (:time (  49 1 1) :key gs6 :dur q)
		    (:time (  49 4 1) :key e4  :dur w+q :amp pp)))

(strummer a5-flute-2 flute-2
	  :events '((:time (t 48  1)  :key ef5 :dur q  :amp p)
		    (:time (t 48  4)  :key e5  :dur et)
		    (:time (t 48  5)  :key f5  :dur et)
		    (:time (t 48  6)  :key e5  :dur et)
		    (:time (t 48  7)  :key f5  :dur et)
		    (:time (t 48  8)  :key gf5 :dur et)
		    (:time (t 48  9)  :key f5  :dur et)
		    (:time (t 48 10)  :key e5  :dur et)
		    (:time (t 48 11)  :key g5  :dur et)
		    (:time (t 48 12)  :key f5  :dur et)
		    (:time (  49 1 1) :key g5  :dur q )
		    (:time (  49 4 1) :key d4  :dur w+q :amp pp)))

(strummer a5-oboe oboe
	  :events '((:time (  47 2 3) :key f4  :dur e  :amp p)
		    (:time (  47 3 1) :key e5  :dur e)
		    (:time (  47 3 3) :key b4  :dur e)
		    (:time (  47 4 1) :key cs5 :dur q)
		    (:time (  48 1 1) :key d5  :dur e)
		    (:time (  48 1 3) :key df5 :dur e)
		    (:time (  48 2 1) :key c5  :dur e)
		    (:time (  48 2 3) :key cs5 :dur q)
		    (:time (  48 3 3) :key d5  :dur q)
		    (:time (  48 4 3) :key ds5 :dur e)
		    (:time (  49 1 1) :key f5  :dur q)
		    (:time (  49 4 1) :key df4 :dur w+q :amp pp)))

(strummer a5-clarinet clarinet
	  :events '((:time (  47 2 3) :key f4  :dur q.  :amp p)
		    (:time (  47 4 1) :key fs4 :dur q)
		    (:time (  48 1 1) :key e4  :dur q)
		    (:time (  48 2 1) :key ds4 :dur e)
		    (:time (  48 2 3) :key e4  :dur q)
		    (:time (  48 3 3) :key f4  :dur q)
		    (:time (  48 4 4) :key fs4 :dur e)
		    (:time (  49 1 1) :key as4 :dur q)
		    (:time (  49 4 1) :key c4  :dur w+q :amp pp)))
		       
(controllers a5-flute-1-dynamics flute-1
	     :events '((:cc   (47 1 1) volume 64)
		       (:time (47 4 1) (48 1 1) t :value 64  96 :ctrl volume :ramp)
		       (:time (48 1 1) (48 4 4) t :value 96 127 :ramp)))

(controllers a5-flute-2-dynamics flute-2
	     :events '((:cc   (47 1 1) volume 96)
		       (:time (48 2 1) (48 4 4) t :value 96 127 :ctrl volume :ramp)))

(controllers a5-oboe-dynamics oboe
	     :events '((:cc   (47 1 1) volume 64)
		       (:time (47 2 3)(47 4 4) t :value 64 96 :ctrl volume :ramp)
		       (:time (48 2 3)(48 4 4) t :value * 127 :ramp)))

(controllers a5-clarinet-dynamics clarinet
	     :events '((:cc   (47 1 1) volume 64)
		       (:time (47 2 3)(47 4 4) t :value 64 127 :ctrl volume :ramp)))


;; Answer 6 Bar 51

(strummer a5-flute-1 flute-1
	  :events '((:time (   51 2 1) :key bf4  :dur q+et :amp p)
		    (:time (t  51   8) :key cs4  :dur et)
		    (:time (t  51   9) :key e4          )
		    (:time (t  51  10) :key ef5  :dur qt)
		    (:time (t  51  12) :key b4   :dur et)
		    (:time (t  52   1) :key e5          )
		    (:time (t  52   2) :key g4          )
		    (:time (t  52   3) :key bf4         )
		    (:time (t  52   4) :key a5          )
		    (:time (t  52   5) :key e5          )
		    (:time (t  52   6) :key c5          )
		    (:time (   52 3 1) :key ds5  :dur s )
		    (:time (   52 3 2) :key d6          )
		    (:time (   52 3 3) :key a5          )
		    (:time (   52 3 4) :key gs5         )
		    (:time (   52 4 1) :key g6   :dur e )
		    (:time (   52 4 3) :key gs6  :dur s )
		    (:time (   52 4 4) :key a6   :dur e )
		    (:time (   53 1 2) :key g6   :dur s )
		    (:time (   53 1 3) :key fs6  :dur e )
		    (:time (   53 2 1) :key d6   :dur s )
		    (:time (   53 2 2) :key ds6  :dur e )
		    (:time (   53 2 4) :key bf5  :dur s )
		    (:time (   53 3 1) :key b5   :dur e )
		    (:time (   53 3 3) :key a5   :dur s )
		    (:time (   53 3 4) :key af5  :dur e )
		    (:time (   53 4 2) :key g6   :dur s )
		    (:time (   53 4 3) :key fs5  :dur e )
		    (:time (t  54   1) :key e5   :dur et)
		    (:time (t  54   2) :key gs4         )
		    (:time (t  54   3) :key bf4         )
		    (:time (   54 2 1) :key a5   :dur e )
		    (:time (   54 2 3) :key e5   :dur s )
		    (:time (   54 2 4) :key ds5  :dur e )
		    (:time (   54 3 2) :key fs4  :dur s )
		    (:time (   54 3 3) :key a4          )
		    (:time (   54 4 1) :key af5         )
		    (:time (   54 4 2) :key ef5         )
		    (:time (   54 4 3) :key a5          )
		    (:time (   54 4 4) :key d5          )
		    (:time (   55 1 1) :key ef5         )
		    (:time (   55 1 2) :key c5          )
		    (:time (   55 1 3) :key bf4         )
		    (:time (   55 1 4) :key a4          )
		    (:time (   55 2 1) :key bf6 :dur h  )))
		    

(strummer a6-flute-2 flute-2
	  :events '((:time (   51 2 1) :key bf4  :dur q+et :amp p)
		    (:time (t  51   8) :key cs4  :dur et)
		    (:time (t  51   9) :key e4          )
		    (:time (t  51  10) :key ef5  :dur qt)
		    (:time (t  51  12) :key bf4  :dur et)
		    (:time (t  52   1) :key e5          )
		    (:time (t  52   2) :key g4          )
		    (:time (t  52   3) :key bf4         )
		    (:time (t  52   4) :key a5          )
		    (:time (t  52   5) :key e5          )
		    (:time (t  52   6) :key c5          )
		    (:time (   52 3 1) :key ds5  :dur s )
		    (:time (   52 3 2) :key d6          )
		    (:time (   52 3 3) :key a5          )
		    (:time (   52 3 4) :key gs5         )
		    (:time (   52 4 1) :key g6   :dur e )
		    (:time (   52 4 3) :key gs6  :dur s )
		    (:time (   52 4 4) :key a6   :dur e )
		    (:time (   53 1 2) :key g6   :dur s )
		    (:time (   53 1 3) :key fs6  :dur e )
		    (:time (   53 2 1) :key d6   :dur s )
		    (:time (   53 2 2) :key ds6  :dur e )
		    (:time (   53 2 4) :key bf5  :dur s )
		    (:time (   53 3 1) :key b5   :dur e )
		    (:time (   53 3 3) :key a5   :dur s )
		    (:time (   53 3 4) :key af5  :dur e )
		    (:time (   53 4 2) :key g6   :dur s )
		    (:time (   53 4 3) :key fs5  :dur e )
		    (:time (   54 1 1) :key a4   :dur q )
		    (:time (   54 2 1) :key gs5  :dur e )
		    (:time (   54 2 3) :key e5   :dur s )
		    (:time (   54 2 4) :key ds5  :dur e )
		    (:time (   54 3 3) :key fs4  :dur s )
		    (:time (   54 3 4) :key a4          )
		    (:time (   54 4 1) :key ef5         )
		    (:time (   54 4 2) :key bf5         )
		    (:time (   54 4 3) :key e5          )
		    (:time (   54 4 4) :key a4          )
		    (:time (   55 1 1) :key af4         )
		    (:time (   55 1 2) :key g4          )
		    (:time (   55 1 3) :key f4          )
		    (:time (   55 1 4) :key bf6 :dur h  )))


(strummer a6-oboe oboe
	  :events '((:time (   52 1 1) :key f4  :dur e   :amp p)
		    (:time (   52 1 3) :key fs4                )
		    (:time (   52 2 1) :key af4                )
		    (:time (   52 2 3) :key g4                 )
		    (:time (t  52   7) :key a4  :dur et        )
		    (:time (t  52   8) :key bf4                )
		    (:time (t  52   9) :key c5                 )
		    (:time (t  52  10) :key fs5 :dur qt        )
		    (:time (t  52  12) :key g5  :dur et        )
		    (:time (t  53   1) :key c5                 )
		    (:time (t  53   2) :key b5                 )
		    (:time (t  53   3) :key f5                 )
		    (:time (   53 2 1) :key c6  :dur e         )
		    (:time (   53 2 3) :key cs6 :dur q         )
		    (:time (   53 3 3) :key d5  :dur s         )
		    (:time (   53 3 4) :key ef5                )
		    (:time (   53 4 1) :key f5                 )
		    (:time (   53 4 2) :key fs5                )
		    (:time (   53 4 3) :key af5                )
		    (:time (   53 4 4) :key g5                 )
		    (:time (   54 1 1) :key bf4 :dur s        )
		    (:time (   54 1 2) :key df4 :dur e        )
		    (:time (   54 1 4) :key e4  :dur s        )
		    (:time (   54 2 1) :key ef5 :dur e        )
		    (:time (   54 2 3) :key d5  :dur q        )
		    (:time (t  54  10) :key d5  :dur et       )
		    (:time (t  54  11) :key c5                )
		    (:time (t  54  12) :key cs5               )
		    (:time (   55 1 1) :key c5  :dur s        )
		    (:time (   55 1 2) :key b4                )
		    (:time (   55 1 3) :key a4                )
		    (:time (   55 1 4) :key af4               )
		    (:time (   55 2 1) :key b5 :dur h         )))
		    

(strummer a6-clarinet clarinet
	  :events '((:time (  52 2 1) :key d4   :dur e  :amp p)
		    (:time (  52 2 3) :key ds4                )
		    (:time (t 52   7) :key e4   :dur et       )
		    (:time (t 52   8) :key f4                 )
		    (:time (t 52   9) :key g4                 )
		    (:time (t 52  10) :key cs5  :dur qt       )
		    (:time (t 52  12) :key a4   :dur et       )
		    (:time (  53 1 1) :key c5   :dur q        )
		    (:time (  53 2 2) :key d5   :dur e.       )
		    (:time (  53 2 4) :key f4                 )
		    (:time (  53 3 1) :key a4   :dur e        )
		    (:time (  53 3 3) :key b4                 )
		    (:time (  53 4 1) :key a4   :dur q        )
		    (:time (  54 1 1) :key bf4  :dur s        )
		    (:time (  54 1 2) :key df4  :dur e        )
		    (:time (  54 1 4) :key f4   :dur s        )
		    (:time (  54 2 1) :key df4  :dur e        )
		    (:time (  54 2 3) :key f4   :dur q        )
		    (:time (  54 3 3) :key g4   :dur e        )
		    (:time (t 54  10) :key a4   :dur et       )
		    (:time (t 54  11) :key b4                 )
		    (:time (t 54  12) :key bf4                )
		    (:time (  55 1 1) :key g4   :dur s        )
		    (:time (  55 1 2) :key fs4                )
		    (:time (  55 1 3) :key e4                 )
		    (:time (  55 1 4) :key ef4                )
		    (:time (  55 2 1) :key gs5  :dur e        )
		    (:time (  55 3 3) :key a6   :dur h+e      )))



