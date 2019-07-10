

(pass? (= 12 (2+ 10)) "2+")
(pass? (= 13 (3+ 10)) "3+")
(pass? (= 14 (4+ 10)) "4+")
(pass? (and (= (limit 5 0 10) 5)
	    (= (limit -1 0 10) 0)
	    (= (limit 11 0 10) 10))
       "limit")

(pass? (and (dividesp 12 4)
	    (not (dividesp 12 5)))
       "dividesp")

(pass? (= (mean '(4 5 6)) 5) "mean")

(let ((value 100)
      (mx -100)
      (mn 1e9))
  (dotimes (i 100)
    (let ((v2 (approximate value :scale 0.1)))
      (setf mx (max mx v2)
	    mn (min mn v2))))
  (pass? (and (<= 90 mn)(<= mx 110)) "approximate"))
 
  

