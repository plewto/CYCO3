

(pass? (and (= +REST+ (dynamic -100))
	    (= +REST+ (dynamic 'r)))
       "dynamic rest")

(let* ((test-list '(r pppp- pppp pppp+
		      ppp- ppp ppp+
		      pp- pp pp+
		      p- p p+
		      mp- mp mp+
		      mf- mf mf+
		      f- f f+
		      ff- ff ff+
		      fff- fff fff+
		      ffff- ffff ffff+))
       (result (dynamic test-list)))
  (pass? (apply #'< result) "dynamic monotonic test")
  (pass? (every #'(lambda (q) (and (plusp q)(<= q 1.0))) (cdr result)) "dynamic range test")
  (pass? (and (every #'dynamic-p test-list)
	      (every #'dynamic-p '(0.5 1.0))
	      (not (dynamic-p 'dog)))
	 "dynamic-p"))

(pass? (and (eq (dynamic-name 'mp) 'mp)
	    (eq (dynamic-name -100.0) 'r)
	    (eq (dynamic-name (float 58/127)) 'mp))
       "dynamic-name")

(pass? (and (= (dynamic->velocity 'r) 0)
	    (= (dynamic->velocity 'pppp-)  2)
	    (= (dynamic->velocity 'ffff+) 127))
       "dynamic->velocity")
