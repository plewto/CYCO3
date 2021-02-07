;;;; CYCO 3 example ex3 "answers"
;;;;

;; NOTE: Use of alternate cue-function
;;

(defun tbar (time-signature time-cue)
  (let ((bar-number (or (car time-cue) 1))
	(tripplet-number (or (second time-cue) 1)))
    (+ (* bar-number (bar-duration time-signature))
       (* 2 tripplet-number (tsubbeat-duration time-signature)))))
    

;; Answer 1  Bar 20
;;
(strummer a1-flute-1 flute-1
	  :cuefn #'tbar
	  :events '((:time (20 5) :key f5  :dur wt+ht+et :amp pp)
		    (:time (21 6) :key e5  :dur et )
		    (:time (21 7) :key ef5         )
		    (:time (21 8) :key d6  :dur ht )))

(strummer a1-flute-2 flute-2
	  :cuefn #'tbar
	  :events '((:time (20 6) :key gs3  :dur 17*et :amp pp)))

(strummer a1-oboe oboe
	  :cuefn #'tbar
	  :events '((:time (20 11) :key b3  :dur 6*qt :amp pp)))

(strummer a1-clarinet clarinet
	  :cuefn #'tbar
	  :events '((:time (20 9) :key df4 :dur 9*et  :amp pp)
		    (:time (21 6) :key ds4 :dur et)
		    (:time (21 7) :key e4  :dur et)
		    (:time (21 8) :key f4  :dur 3*et)))

(controllers a1-flute-dynamics (list flute-1 oboe)
	     :events '((:time (20 t2 1)(22 1 1) t :value 127 16 :ctrl volume :ramp)
		       (:cc   (22 1 1) volume 127)))

;; Answer 2 Bar 26
;;
(strummer a2-flute-1 flute-1
	  :cuefn #'bar
	  :events '((:time (26 t5)  :key df6 :dur ht+h.  :amp pp )
		    (:time (27 4 1) :key c6  :dur h   )
		    (:time (28 2 1) :key b5  :dur h   )
		    (:time (28 4 1) :key bf5 :dur e   )
		    (:time (28 4 3) :key a5  :dur e   )
		    (:time (29 1 1) :key af5 :dur q   )))

(strummer a2-flute-2 flute-2
	  :cuefn #'bar
	  :events '((:time (26 t5)   :key f5  :dur ht+q  :amp pp+)
		    (:time (27 2  1) :key g5  :dur 2*w)))

(strummer a2-oboe oboe
	  :cuefn #'bar
	  :events '((:time (26 t5)  :key c5  :dur ht+h.  :amp pp+)
		    (:time (27 4 1) :key b4  :dur h)
		    (:time (28 2 1) :key c5  :dur w)))

(strummer a2-clarinet clarinet
	  :cuefn #'bar
	  :events '((:time (26 t5)  :key fs4 :dur ht+q  :amp pp+)
		    (:time (27 2 1) :key gs4 :dur 2*h.)
		    (:time (28 4 1) :key a4  :dur e   )
		    (:time (28 4 3) :key as4 :dur e   )
		    (:time (29 1 1) :key b4  :dur q   )))

;; Answer 3 Bar 34 
;;

(strummer a3-flute-1 flute-1
	  :cuefn #'bar
	  :events '((:time (34 1 1) :key a5  :dur w  :amp  p)
		    (:time (35 2 1) :key af5 :dur q.)
		    (:time (35 3 4) :key g5  :dur e )
		    (:time (35 4 1) :key gf5 :dur q )
		    (:time (36 1 1) :key f5  :dur q)
		    (:time (36 2 1) :key f5  :dur h )))

(strummer a3-flute-2 flute-2
	  :cuefn #'bar
	  :events '((:time (34 1 1) :key b4  :dur h.  :amp p)
		    (:time (35 1 1) :key c5  :dur w+q)
		    (:time (36 2 1) :key c5  :dur h)))

(strummer a3-oboe oboe
	  :cuefn #'bar
	  :events '((:time (34 3 1) :key c5  :dur q   :amp p)
		    (:time (34 4 1) :key g5  :dur q)
		    (:time (35 1 1) :key f5  :dur q)
		    (:time (35 2 1) :key e5  :dur h.+h)
		    (:time (36 3 1) :key e5  :dur e)))

(strummer a3-clarinet clarinet
	  :cuefn #'bar
	  :events '((:time (34 2 1) :key gs4 :dur w  :amp p)
		    (:time (35 2 1) :key a4  :dur q )
		    (:time (35 3 1) :key b4  :dur q.)
		    (:time (35 4 3) :key d5  :dur e )
		    (:time (36 1 1) :key cs5 :dur h)
		    (:time (36 3 1) :key cs5 :dur e)))
		    
(controllers a3-dynamics (list flute-1 flute-2 oboe clarinet)
	     :cuefn #'bar
	     :events '((:cc (34 1 1) volume 112)
		       (:time (35 3 3)(36 1 1) t :value 112 127 :ctrl volume :ramp)
		       (:time * (36 4 1) :value 127 32 :ramp)
		       (:cc (37 1 1) volume 127)))


;; Answer 4 Bar 41

(strummer a4-flute-1 flute-1
	  :cuefn #'bar
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
	  :cuefn #'bar
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
	  :cuefn #'bar
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

(defchord '[gliss] '(0 1 3 7 10 12))
(defchord '[gliss] '(0 0 1 2 2 3 4 5 6 6 7 8 8 9 10 10 11 12 12 12 12 12 12 12 12))


(strummer a4-clarinet clarinet
	  :cuefn #'bar
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
	     :cuefn #'bar
	     :events '((:cc (41 1 1) volume 100)
		       (:time (42 1 1) (41 1 1) t :value 100 127 :ctrl volume :ramp)
		       (:time * (43 2 1) t :value 100 127 :ramp)
		       (:cc (44 1) volume 127)))
	  
	  
;; (strummer a4-gliss-test clarinet
;; 	  :cuefn #'bar
;; 	  :events '((:time ( 1 2 1) :key gs4  :dur h  :amp p)
;; 		    (:time ( 1 4 1) :key e4   :dur q)
;; 		    (:time ( 2 1 1) :key d4   :dur q)
;; 		    (:time ( 2 2 1) :key cs4  :dur e)
;; 		    (:time ( 2 2 3) :key d4   :dur e)
;; 		    (:time ( 2 3 1) :key b4   :dur e)
;; 		    (:time ( 2 3 3) :key c5   :dur e)
;; 		    (:time ( 2 4 1) :key a4   :dur q)
;; 		    (:time ( 3 1 1) :key ds4  :chord [gliss] :dur t :strum tt :end-together nil :amp p-)
;; 		    (:time ( 3 1 4) :key ds5  :chord [solo] :dur h+s :amp p)))  
  
