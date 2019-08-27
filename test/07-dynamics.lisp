;;;; test 07 dynamics
;;;;

(pass? "rest" (= (dynamic -100)(dynamic 'r) +REST+))

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
  (pass? "monotonic" (apply #'< result))
  (pass? "range" (every #'(lambda (q)
			    (and (numberp q)
				 (plusp q)
				 (<= q 1.0)))
			(cdr result)))
  (pass? "dynamic-p" (and (every #'dynamic-p test-list)
			  (every #'dynamic-p result)
			  (not (dynamic-p 'dog)))))

(pass? "dynamic-name"
       (and (eq (dynamic-name 'mp) 'mp)
	    (eq (dynamic-name -100.0) 'r)
	    (eq (dynamic-name (float 58/127)) 'mp)))

(pass? "dynamic->velocity"
       (and (zerop (dynamic->velocity 'r))
	    (= (dynamic->velocity 'pppp-) 2)
	    (= (dynamic->velocity 'ffff+) 127)))
