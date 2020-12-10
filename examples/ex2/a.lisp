;;;; CYCO examples ex2 a.lisp
;;;;

(section a :bars 8)

(let ((cue-list '((1 1 1)(1 1 3)(1 4 1)(1 4 3)
		  (2 1 1)(2 1 3)(2 4 1)(2 4 3)
		  (3 1 1)(3 1 3)(3 4 1)(3 4 3)
		  (4 1 1)(4 1 3)(4 4 1)(4 4 3)
		  (5 1 1)(5 1 3)(5 3 3)(5 4 1)(5 4 3)
		  (6 1 1)(6 1 3)(6 4 1)(6 4 3)
		  (7 1 1)(7 1 3)(7 4 1)(7 4 3)
		  (8 1 1)(8 1 3)(8 4 1)(8 4 3))))

  (qball a-bass piano
	 :cue cue-list
	 :key '(f3 f3 f3 f3
		   fs3 fs3 fs3 fs3
		   f3 f3 f3 f3
		   fs3 fs3 fs3 fs3
		   f3 f3 f3 fs3 g3
		   gs3 gs3 gs3 gs3
		   ds3 ds3 ds3 ds3
		   fs3 fs3 fs3 fs3)
	 :amp 'fff
	 :dur 'e)

  (qball a-kick gm-kick
	 :cue cue-list
	 :key '(x1 x2 x1)
	 :reset-on-repeat nil
	 :amp 'f ))

(qball a-snare gm-snare
       :bars 1
       :cue '((1 2 1)(1 4 1))
       :key (dice :of '(x1 x1 x1 x2))
       :amp 'pp)
       
(qball a-shaker gm-shaker
       :bars 1
       :cue '((1 1 1)(1 1 3)(1 2 1)(1 2 3)
	      (1 3 1)(1 3 3)(1 4 1)(1 4 3))
       :key (dice :of (cons 'maracas (copies 8 'cabasa)))
       :amp 'p)



(->midi a)
  
