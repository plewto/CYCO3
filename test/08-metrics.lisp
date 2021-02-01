;;;; test 08 metrics
;;;;

(pass? "metric-p 8.1" 
       (and (every #'metric-p  '(-1 0 1 2 4 8 16
				    r z x t s e q h w
				    z. x. t. s. e. q. h. w. q.. q...
				    zt xt tt st et qt ht wt qtt qttt))
	    (not (some #'metric-p '(ape qe "q" "q." "qt")))))

(pass? "metric (numeric) 8.2"
       (and (= (metric -100) -1)
	    (= (metric 0) 0)
	    (= (metric 1/2) 1/2)
	    (= (metric 3.14) 3.14)))
       

(pass? "metric (symbolic) 8.3"
 (and (= (metric 'r) -1)
	    (= (metric 'e)(/ (metric 'q) 2))
	    (= (metric 'q.)(* 3/2 (metric 'q)))
	    (= (metric 'q..)(* 9/4 (metric 'q)))
	    (= (metric 'qt)(* 2/3 (metric 'q)))
	    (< (abs (- (metric 'qtt)(* 4/9 (metric 'q)))) 0.00001)))  ;; expect rounding error

(pass? "metric-expression-p 8.4"
       (and (every #'metric-expression-p '(-1000 1 3.14 r e q q. qt q+e q+e-st w+h.-qtt+e... 2*w 2*w+h))
	    (not (some #'metric-expression-p '(ape a w+q*3 3*4*w)))))

(pass? "metric-expression 8.5"
       (and (= (metric-expression 'w)(metric 'w))
	    (= (metric-expression 'w.+q..)(+ (metric 'w.)(metric 'q..)))
	    (= (metric-expression '3*wt+h.)(* 3 (+ (metric 'wt)(metric 'h.))))
	    (equal (metric-expression '(w w+h. 3*w.+h))
		   (list (metric 'w)
			 (+ (metric 'w)(metric 'h.))
			 (* 3 (+ (metric 'w.)(metric 'h)))))))
