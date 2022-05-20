;;;; CYCO comprehension
;;;;
;;;; Converts s-expression to nested pattern/generator structure
;;;; Example: 
;;;;   (pattern-comprehension '(cycle :of (1 2 3 (line :of (ape bat cat)))))
;;;; returns identical structure as the expression
;;;;   (cycle :of (list 1 2 3 (line :of '(ape bat cat))))
;;;; 
;;;; 
;;;; Restrictions:
;;;;
;;;; 1) May use any pattern/generator contained in commands.
;;;; 2) Special keyword symbols may not appear as pattern data
;;;;     I.E.  '(CYCLE :of '(odd))  is not allowed
;;;; 3) May not use anonymous functions
;;;;     I.E   '(COUNTER 1 100 :HOOK #'(LAMBDA (N)(+ 100 N)))  is not allowed
;;;; 4) Pre defined functions may be used
;;;;     (defun foo (n)(+ n 100))
;;;;     '(COUNTER 1 100 :HOOK #'foo) is allowed
;;;;

(let ((commands '("(BAG" "(CYCLE" "(DICE" "(LINE" "(WALKER"
		  "(BONES" "(COUNTER" "(HAILSTONE" "(LOGISTIC"
		  "(RAMP" "(RACAMAN" "(SHIFT-REGISTER"))
      (keywords '("OF" "FUNCTION" "HOOK" "LENGTH" "BY"
		  "EVEN" "ODD" "PRERUN" "MU" "MASK")) )

  (labels ((command-p (word)
		      (member word commands :test #'string=))

	   (keyword-p (word)
		      (member word keywords :test #'string=))

	   (car-p (word)
		(char= (char word 0) #\())  

	   (function-p (word)
		       (and (> (length word) 2)
			    (char= (char word 0) #\#)
			    (char= (char word 1) #\')))
	   
	   (exp->string (exp)
			(let ((sexp (split-string (sformat "~A" exp)))
			      (acc ""))
			  (dolist (word sexp)
			    (cond ((command-p word)
				   (setf acc (sformat "~A ~A" acc word)))
				  ((car-p word)
				   (setf acc (sformat "~A (list '~A" acc (subseq word 1))))
				  ((keyword-p word)
				   (setf acc (sformat "~A :~A " acc word)))
				  ((function-p word)
				   (setf acc (sformat "~A ~A " acc word)))
				  (t (setf acc (sformat "~A '~A" acc word)))))
			  acc)) )

	  (defun pattern-comprehension (exp)
	    (let ((str (exp->string exp)))
	      (eval (read-from-string str)))) ))
