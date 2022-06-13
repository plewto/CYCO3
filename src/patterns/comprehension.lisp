;;;; CYCO comprehension
;;;;
;;;; Converts symbol to nested pattern/generator structure
;;;; 
;;;; Example
;;;;   (pattern-comprehension '(cycle (a b (line (ape bat))))
;;;; Returns identical object to (cycle :of (list 'a 'b (line :of '(ape bat))))
;;;;
;;;; Bindings to non-quoted symbols are included by use of { } brackets
;;;;
;;;; (param animal 'dog)
;;;; (pattern-comprehension '(cycle (a b {animal} c)))
;;;; is identical to (cycle :of (list 'a 'b animal 'c))
;;;;
;;;; {} Brackets may be used for any escaped expression
;;;; (pattern-comprehension (counter 10 20 :by 2 :hook {#'(lambda (n)(+ 100 n))})
;;;; Is identical to (counter 10 20 :by 2 :hook #'(lambda (n)(+ 100 n)))
;;;; 


(in-package :cyco)

(let ((escape-open "{")
      (escape-close "}")
      (commands (let ((htab (make-hash-table :size 12 :test #'equal)))
		  (setf (gethash "BAG" htab) "BAG :OF")
		  (setf (gethash "CYCLE" htab) "CYCLE :OF")
		  (setf (gethash "DICE" htab) "DICE :OF")
		  (setf (gethash "LINE" htab) "LINE :OF")
		  (setf (gethash "WALKER" htab) "WALKER :OF")
		  (setf (gethash "BONES" htab) "BONES")
		  (setf (gethash "COUNTER" htab) "COUNTER")
		  (setf (gethash "HAILSTONE" htab) "HAILSTONE")
		  (setf (gethash "LOGISTIC" htab) "LOGISTIC")
		  (setf (gethash "RECAMAN" htab) "RECAMAN")
		  (setf (gethash "RAMP" htab) "RAMP")
		  (setf (gethash "SHIFT-REGISTER" htab) "SHIFT-REGISTER")
		  htab)) 
      (reserved-keywords '("FINAL" "OF" "FUNCTION" "EVEN" "ODD" "LENGTH"
			   "BY" "MASK" "PRERUN" "HOOK"))
      (docs "Pattern comprehension converts symbols to pattern/generator structures.

The following pattern and generator types may be used

      BAG CYCLE DICE LINE WALKER BONES COUNTER HAILSTONE LOGISTIC RECAMAN RAMP
      and SHIFT-REGISTER

Example 1 (simple pattern)

     (pattern-comprehension '(cycle (a b c)))
     Returns identical object to (cycle :of '(a b c))
     :of keyword and quoting the element list are not necessary.

Example 2 (nested patterns)
      
     (pattern-comprehension '(cycle (a b (line (ape bat)))))
     Returns identical object as (cycle :of (list 'a 'b (line :of '(ape bat))))

Example 3 (reserved keywords)

     Only the following keywords may be used, either as keyword arguments
     or in a pattern's element list,

     :BY :EVEN :FINAL :FUNCTION :HOOK :LENGTH :ODD :OF and :PRERUN.

     both '(counter 0 10 :by 2) and '(cycle (x y :by))  are valid.

     BEWARE that any other keyword in an element list will be converted 
     to an ordinary symbol

     '(cycle (1 2 :ape))  --> 1 2 ape 1 2 ape...

Example 4 (Curly brackets) 
      
    Curly brackets { } may be used to insert literal code.

    (defun fn (n)(+ m 100))
    (param p1 (pattern-comprehension '(counter 10 20 :hook {#'fn})))
    (next p1 5) --> 110 111 112 113 114 

    (param p2 (pattern-comprehension '(counter 10 20 :hook {#'(lambda (n)(* 3 n))})))
    (next p2 5  --> 30 33 36 39 42

    BEWARE Do not place a keyword immediately to the right of an open curly bracket
    as in {:hook, doing so is a read-error, lisp tries to locate a package named '{'.


    (param a 'ape)
    (param p3 (pattern-comprehension '(cycle (x y {a} z))))
    (next p3 3) --> x y ape z"))
  
  (labels ((trim-white (word)
		       (string-trim '(#\space #\tab #\newline) word))
	   
	   (command-p (word)
		      (gethash word commands))
	 
	   (car-p (word)
	   	  (char= (char word 0) #\())

	   ;; CREDIT for replace-all from http://cl-cookbook.sourceforge.net/strings.html#manip
	   ;;
	   (replace-all (string part replacement &key (test #'char=))
	     (with-output-to-string (out)
				    (loop with part-length = (length part)
					  for old-pos = 0 then (+ pos part-length)
					  for pos = (search part string
							    :start2 old-pos
							    :test test)
					  do (write-string string out
							   :start old-pos
							   :end (or pos (length string)))
					  when pos do (write-string replacement out)
					  while pos)))
	   
	   ;; place spaces around all brackets.
	   ;;
	   (pre-process (exp)
			(let ((acc ""))
			  (loop for c across exp do
				(if (member c '(#\( #\) #\{ #\}) :test #'char=)
				    (setf acc (sformat "~A ~A " acc c))
				  (setf acc (sformat "~A~A" acc c))))
			  (string-upcase acc)))

	   ;; fix formatting for #' syntax.
	   ;;
	   (post-process (str)
			 (dolist (pair '(("#  '  " . "#'")
					 ("#  '" . "#'")))
			   (setf str (replace-all str (car pair)(cdr pair))))
			 str)

	   ;; Parse out text between { and } brackets.
	   ;; Returns two values: 1) The text
	   ;;                     2) Modified words list.
	   (span-escape (words)
			(let ((acc "")
			      (word ""))
			  (while (and words (not (string= word escape-close)))
			    (setf acc (sformat "~A ~A " acc word))
			    (setf word (car words))
			    (setf words (cdr words)))
			  (values acc words))) 

	   ;; Converts symbolic expression to string.
	   ;;
	   (parse (exp)
		  (let* ((str (pre-process (sformat "~A" exp)))
			 (words (split-string str))
			 (word "")
			 (next-word "")
			 (acc ""))
		    (while words
		      (setf word (trim-white (car words)))
		      (setf words (cdr words))
		      (setf next-word (and words (trim-white (car words))))
		      (cond
		       ((string= word "(")
			(if (command-p next-word)
			    (setf acc (sformat "~A (" acc))
			  (setf acc (sformat "~A (list " acc))))
			    
		       ((command-p word)
			(setf acc (sformat "~A ~A " acc (gethash word commands))))
		       ((equal word "OF")  ;; ignore optional :OF keyword
			nil) 
		       ((member word reserved-keywords :test #'string=)
			(setf acc (sformat "~A :~A " acc word)))
		       ((string= word escape-open)
			(format t "string= escape-open~%")
			(multiple-value-bind (s w)(span-escape words)
					     (setf acc (sformat "~A ~A" acc s))
					     (setf words w)))
		       ((and (plusp (length word))
			     (not (member word '("(" ")") :test #'string=)))
			(let ((token (read-from-string word)))
			  (if (keywordp token)
			      (setf acc (sformat "~A :~A " acc token))
			    (setf acc (sformat "~A '~A " acc token)))))
		       (t
			(setf acc (sformat "~A ~A" acc word)))))
		    acc)) )

	  (defun pattern-comprehension (exp)
	    (let* ((str (post-process (parse exp))))
	      (eval (read-from-string str))))

	  (defun pattern-comprehension-p (item)
	    "Returns non-nil if argument defines a pattern-comprehension."
	    (and (listp item)
		 (gethash (->string (car item)) commands)))

	  (setf (documentation 'pattern-comprehension 'function) docs) ))
