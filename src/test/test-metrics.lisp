

(pass? (and (every #'metric-p '(-1 0 1 2 4 8 16
				   r z x t s e q h w
				   z. x. t. s. e. q. h. w. q.. q...
				   zt xt tt st et qt ht wt qtt qttt))
	    (none #'metric-p '(ape qe "q" "q." "qt")))
       "metric-p")

(pass? (and (= (metric -100) -1)
	    (= (metric 0) 0)
	    (= (metric 1/2) 1/2)
	    (= (metric 3.14) 3.14))
       "metric (number)")

(pass? (and (= (metric 'r) -1)
	    (= (metric 'e)(/ (metric 'q) 2))
	    (= (metric 'q.)(* 3/2 (metric 'q)))
	    (= (metric 'q..)(* 9/4 (metric 'q)))
	    (= (metric 'qt)(* 2/3 (metric 'q)))
	    (< (abs (- (metric 'qtt)(* 4/9 (metric 'q)))) 0.00001)  ;; some rounding error
	    )
       "metric (symbol)")
			      

(pass? (every #'metric-p (metric '(-100 r e q h w))) "metric (list)")


(pass? (and (every #'metric-expression-p
		   '(-100 1 3.14 r e q q. qt q+e q+e-st w+h.-qtt+e... 2*w 2*w+h-q))
	    (none #'metric-expression-p '(ape a w+q*3 3*4*w)))
       "metric-expression-p")

(pass? (and
	(= (metric-expression 'w)(metric 'w))
	(= (metric-expression 'w.+q..)(+ (metric 'w.)(metric 'q..)))
	(= (metric-expression '3*w.-qt)(* 3 (- (metric 'w.)(metric 'qt))))
	(same-thing-p (metric-expression '(w w.+q.. 3*w.-qt))
		      (list (metric 'w)
			    (+ (metric 'w.)(metric 'q..))
			    (* 3 (- (metric 'w.)(metric 'qt))))))
       "metric-expression")
