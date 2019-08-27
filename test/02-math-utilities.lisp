;; test 02-math-utilities


(pass? "2+" (= (2+ 10) 12) "")
(pass? "3+" (= (3+ 10) 13) "")
(pass? "4+" (= (4+ 10) 14) "")

(pass? "limit" (and (= (limit 5 0 10) 5)
		    (= (limit -1 0 10) 0)
		    (= (limit 11 0 10) 10)))

(pass? "dividesp" (and (dividesp 12 4)
		       (not (dividesp 12 5))))

(pass? "mean" (= (mean '(4 5 6)) 5))

(let ((value 100)
      (mx -1e9)
      (mn 1e9))
  (dotimes (i 100)
    (let ((v2 (approximate value :scale 0.1)))
      (setf mx (max mx v2)
	    mn (min mn v2))))
  (pass? "approximate" (and (<= 90 mn)(<= mx 110))))
