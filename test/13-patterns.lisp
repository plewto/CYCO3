;;;; test 13 patterns
;;;;

(let* ((pat (line :of '(1 2 3)))
       (cln (clone pat)))
  (pass? "Line pattern test 13.1"
	 (and 
	  (equal (next pat 6) '(1 2 3 3 3 3))
	  (progn
	    (reset pat)
	    (= (next pat) 1))
	  (equal (next cln 6) '(1 2 3 3 3 3)))))

(let* ((pat (cycle :of '(a b c)))
       (cln (clone pat)))
  (pass? "Cycle pattern test 13.2"
	 (and
	  (equal (next pat 5) '(a b c a b))
	  (progn
	    (reset pat)
	    (eq (next pat) 'a))
	  (equal (next cln 6) '(a b c a b c)))))

(let* ((elements '(a b c))
       (pat (bag :of elements :final 'final))
       (cln (clone pat))
       (seq1 (next pat 5))
       (seq2 (next cln 5))
       (possible '((a b c final final)(a c b final final)
		   (b a c final final)(b c a final final)
		   (c a b final final)(c b a final final))))
  (pass? "Bag test 13.3"
	 (and (member seq1 possible :test #'equal)
	      (member seq2 possible :test #'equal))))

(let* ((elements '(a b c))
       (pat (bag :of elements :final (line :of elements)))
       (possible '((a b c a b c c)(a c b a b c c)
		   (b a c a b c c)(b c a a b c c)
		   (c a b a b c c)(c b a a b c c))))
       (pass? "Bag test 13.4"
	      (member (next pat 7) possible :test #'equal)))

(let* ((w (walker :of '(0 1 2 3 4 5 6 7 8 9)))
       (seq (next w 100))
       (diff '())
       (previous (car seq)))
  (dolist (v (cdr seq))
    (cond ((zerop v)
	   (push (if (or (= previous 9)(= previous 1)) 1 100) diff))
	  ((= v 9)
	   (push (if (or (zerop previous)(= previous 8)) 1 100) diff))
	  (t (push (abs (- v previous)) diff)))
    (setf previous v))
  (pass? "Walker test 13.5"
	 (every #'(lambda (n)(= n 1)) diff)))

(labels ((fib (n)
	      (if (< n 1)
		  1
		(+ (fib (- n 1))
		   (fib (- n 2))))))
  (let ((pat (wrapper :of #'fib)))
    (pass? "Wrapper test 13.6"
	   (equal (next pat 10) '(1 2 3 5 8 13 21 34 55 89)))))

(let* ((a (cycle :of '(a b c)))
       (b (line :of '(x y z)))
       (s (switch :of (list a b)))
       (seq '()))
  (select s 0)
  (dotimes (i 6)
    (push (next-1 s) seq))
  (select s 1)
  (dotimes (i 6)
    (push (next-1 s) seq))
  (pass? "Switch test 13.7"
	 (equal (reverse seq) '(a b c a b c z z z z z z)))
  (reset s)
  (setf seq '())
  (setf (step-only-selected s) t)
  (select s 0)
  (dotimes (i 6)
    (push (next-1 s) seq))
  (select s 1)
  (dotimes (i 6)
    (push (next-1 s) seq))
  (pass? "Switch test 13.8"
	 (equal (reverse seq) '(a b c a b c x y z z z z))))
       
       

