;;;; test 05 keynumbers


(pass? "keynumber 5.1"
       (and (= (keynumber -100) +REST+)
	    (= (keynumber 'r)   +REST+)
	    (= (keynumber 60)  60)
	    (= (keynumber 'c) 0)
	    (= (keynumber 'c5) 60)
	    (= (keynumber 200) 116) ;; out of bounds should auto transpose
	    (equal (keynumber '(r c5 e5 g5 (c6 e6 g6))) '(-1 60 64 67 (72 76 79))) ))

(defkeynumber 'keyfoo 64)
(pass? "defkeynumber 5.2" (eq (keynumber 'keyfoo) 64))
(pass? " keynumber-p 5.3" (every #'keynumber-p '(c c c5 cs5 df5 0 60 127)))
(pass? "~keynumber-p 5.4" (not (some #'keynumber-p '(ape 27/12 "bar" (nested)))))
(pass? " rest-p 5.5" (every #'rest-p '(-1000 -1 r)))
(pass? "~rest-p 5.6" (not (some #'rest-p '(0 c4 123))))

(pass? "pitch-class 5.7"
       (and (= -1 (pitch-class -100)(pitch-class 'r))
	    (every #'zerop (pitch-class '(0 c0 c1 c2 60 72)))
	    (every #'(lambda (q)(= q 1)) (pitch-class '(1 cs cs0 cs1 cs2 61 73)))
	    (equal (pitch-class '(r c cs d ds e f fs g gs a as b))
		               '(-1 0 1 2 3 4 5 6 7 8 9 10 11)) ))
(pass? "octave 5.8"
       (and (= -1 (octave -1000)(octave 'r))
	    (= (octave 0) 0)
	    (equal (octave '(r c0 cs2 d3 ds4 e5 f6)) '(-1 0 2 3 4 5 6))))

(pass? "keyname 5.9"
      (and (eq (keyname -100) 'r)
	   (eq (keyname 'r) 'r)
	   (eq (keyname 'c5) 'c5)
	   (eq (keyname 60) 'c5)
	   (equal (keyname '(r 0 cs1 60)) '(r c0 cs1 c5))))

(pass? "transpose 5.10"
       (and (= (keynumber (transpose 'c5 -12))(keynumber 'c4))
	    (= (pitch-class (transpose 'c5 -120))(pitch-class 'c5))
	    (= (pitch-class (transpose 'c5 +120))(pitch-class 'c5))
	    (rest-p (transpose 'r 15))
	    (equal (keynumber (transpose '(c0 c1 c2) 12))(keynumber '(c1 c2 c3)))))

(pass? "invert 5.11"
       (and (= (invert 61 60) 59)
	    (= (invert 59 60) 61)
	    (rest-p (invert -100 60))
	    (equal (keynumber (invert '(60 61 62) 60)) '(60 59 58))))

(pass? "white-key-p 5.12"
       (and (white-key-p 60)
	    (not (white-key-p 61))
	    (every #'white-key-p '(c5 d5 e5 f5 g5 a5 b5 c6))
	    (not (every #'white-key-p '(c5 d5 e5 f5 g5 a5 b5 c6 cs6)))))

(pass? "black-key-p 5.13"
       (and (not (black-key-p 60))
	    (black-key-p 61)
	    (every #'black-key-p '(cs1 ds1 fs1 gs1 as1))
	    (not (every #'black-key-p '(cs1 ds1 fs1 gs1 as1 c2)))))

(pass? "white-keys 5.14"
       (equal (white-keys 'c1 'c2) '(12 14 16 17 19 21 23 24)))

(pass? "black-keys 5.15"
       (equal (black-keys 'c1 'c2) '(13 15 18 20 22)))


