


(pass? (and (= (keynumber -100) +REST+)
	    (= (keynumber 'r)   +REST+)
	    (= (keynumber 60)  60)
	    (= (keynumber 'c) 0)
	    (= (keynumber 'c5) 60)
	    (= (keynumber 200) 116) ;; out of bounds should auto transpose
	    (same-thing-p (keynumber '(r c5 e5 g5 (c6 e6 g6))) '(-1 60 64 67 (72 76 79))))
       "keynumber")
	    
(defkeynumber 'key-foo 64)
(pass? (eq (keynumber 'key-foo) 64) "defkeynumber")
(pass? (every #'keynumber-p '(r c c5 cs5 df5 0 60 127)) "keynumber-p")
(pass? (none #'keynumber-p '(ape 27/12 "bat" (nested-list))) "not keynumber-p")
(pass? (every #'rest-p '(-1000 -1 r)) "rest-p")
(pass? (none #'rest-p '(0 c4 123)) "not rest-p")

(pass? (and (= -1 (pitch-class -100)(pitch-class 'r))
	    (every #'(lambda (q)(zerop q)) (pitch-class '(0 c0 c1 c2 c3 c4 c5 c6)))
	    (every #'(lambda (q)(= q 1)) (pitch-class '(1 cs0 cs1 cs2 cs3 cs4 cs5 cs6)))
	    (same-thing-p (pitch-class '(r c cs d ds e f fs g gs a as b)) '(-1 0 1 2 3 4 5 6 7 8 9 10 11)))
       "pitch-class")
	    
(pass? (and (= -1 (octave -1000)(octave 'r))
	    (= (octave 0) 0)
	    (same-thing-p (octave '(r c0 cs1 d1 e1 f1 g1 a1 b1)) '(-1 0 1 1 1 1 1 1 1)))
       "octave")
	    

(pass? (and (eq (keyname -100) 'r)
	    (eq (keyname 'r) 'r)
	    (eq (keyname 'c5) 'c5)
	    (eq (keyname 60) 'c5)
	    (same-thing-p (keyname '(r 0 cs1 60)) '(r c0 cs1 c5)))
       "keyname")

(pass? (and (= (keynumber (transpose 'c5 -12))(keynumber 'c4))
	    (= (pitch-class (transpose 'c5 (* -10 12)))(pitch-class 'c5))
	    (= (pitch-class (transpose 'c5 (* 10 12)))(pitch-class 'c5)))
       "transpose")

(pass? (and (= (invert 61 60) 59)
	    (= (invert 59 60) 61)
	    (rest-p (invert -100 60)))
       "invert")

(pass? (and (every #'white-key-p '(0 c0 d1 e2 f3 g4 a5 b6))
	    (not (every #'white-key-p '(0 c0 d1 e2 f3 g4 a5 b6 cs4))))
       "white-key-p")

(pass? (and (every #'black-key-p '(cs2 df2 ds2 ef2 fs2 gf2 gs2 af2 as2 bf2))
	    (not (every #'black-key-p '(cs2 df2 ds2 ef2 fs2 gf2 gs2 af2 as2 bf2 c4))))
       "black-key-p")

(pass? (same-thing-p (white-keys 'c1 'c2) '(12 14 16 17 19 21 23 24))
       "white-keys")

(pass? (same-thing-p (black-keys 'c1 'c2) '(13 15 18 20 22))
       "black-keys")


