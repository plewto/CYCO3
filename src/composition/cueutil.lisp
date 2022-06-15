;;;; cyco composition/cueutil.lisp
;;;;
;;;; Cuelist related utilities.
;;;;

(in-package :cyco)

(labels ((compress (str)
		   (let ((white '(#\space #\newline #\tab)))
		     (remove-if #'(lambda (c)(member c white :test #'char=)) str)))

	 (pad-zeros (str width)
		    (string-pad-right str width #\0))

	 (invert-cuestring (str)
			   (let ((s "")
			   	 (c nil))
			     (dotimes (i (length str))
			       (setf c (char str i))
			       (setf s (str+ s (cond ((char= c #\0) "1")
			   			     ((char= c #\1) "0")
			   			     (t c)))))
			     s))
	
	 
	 (simplify-cue-point (cp)
			      (let* ((width 3)
				     (diff (- width (length cp))))
				(if (plusp diff)
				    (append cp (copies diff 1))
				  (subseq cp 0 width))))

	 (binary-char-p (c)
			(or (char= c #\0)
			    (char= c #\1)))
	 
	 (print-header (text)
		       (if text (format t ";; ~A" text)))
	  
	 (print-bar (bar-number)
		    (format t "~%;; BAR ~3D : " bar-number))

	 (-pprint-cuestring (cuestring timesig use-subbeats)
			    (let* ((tsig (select-time-signature timesig))
				   (group (property tsig (if use-subbeats :subbeats :tsubbeats)))
				   (bar-length (* group (property tsig :beats)))
				   (event-count (* bar-length (property tsig :bars)))
				   (cstr (pad-zeros (cuelist cuestring :form :binary :timesig tsig
							     :use-subbeats use-subbeats) event-count))
				   (bar-count 1))
			      (dotimes (i (length cstr))
				(if (zerop (rem i bar-length))
				    (progn
				      (format t "~%;; BAR ~3D :" bar-count)
				      (setf bar-count (1+ bar-count))))
				(if (zerop (rem i group))(format t " "))
				(format t "~A" (char cstr i))))
			    (format t "~%"))

	 (-pprint-cuelist (cuelist)
			   (let ((max-line-length 8)
				 (current-bar (caar cuelist))
				 (line-length 0))
			     (print-bar current-bar)
			     (dolist (q cuelist)
			       (if (not (equal (car q) current-bar))
				   (progn
				     (setf current-bar (car q))
				     (setf line-length 0)
				     (print-bar current-bar)))
			       (if (> line-length max-line-length)
				   (progn
				     (format t "~%;;           ")
				     (setf line-length 0)))
			       (format t "~A" q)
			       (setf line-length (1+ line-length))))
			   (format t "~%"))
			     
	 
	 (cuelist->cuestring (cuelist timesig use-subbeats)
			     (setf cuelist (mapcar #'simplify-cue-point cuelist))
			     (let* ((tsig (select-time-signature timesig))
				    (gamut (mapcar #'simplify-cue-point (cue-gamut tsig (not use-subbeats))))
				    (cstr ""))
			       (dotimes (i (length gamut))
				 (let ((cp (nth i gamut)))
				   (setf cstr (str+ cstr (if (member cp cuelist :test #'equal) "1" "0")))))
			       (pad-zeros cstr (length gamut))))

	 (cuestring->cuelist (cuestring timesig use-subbeats)
			     (let ((cstr (compress cuestring)))
			       (if (notevery #'binary-char-p cstr)
				   (cyco-error "Invalid binary-cuestring" cuestring))
			       (let* ((tsig (select-time-signature timesig))
				      (gamut (cue-gamut tsig (not use-subbeats)))
				      (cuelist '()))
				 (dotimes (i (min (length gamut)(length cstr)))
				   (if (char= (char cstr i) #\1)
				       (push (nth i gamut) cuelist)))
				 (reverse cuelist))))



	 (shift-cuestring-right (cuestring n)
				(if (> n (length cuestring))
				    (scopies (length cuestring) "0")
				  (let ((head (scopies n "0"))
					(tail (subseq cuestring 0 (- (length cuestring) n))))
				      (str+ head tail))))
	 
	 (shift-cuestring-left (cuestring n)
			       (setf n (abs n))
			       (if (> n (length cuestring))
				   (scopies (length cuestring) "0")
				 (let ((head (subseq cuestring n (length cuestring)))
				       (tail (scopies n "0")))
				   (str+ head tail))))

	 (shift-cuestring (cuestring n)
			  (cond ((null n)
				 cuestring)
				((zerop n)
				 cuestring)
				((plusp n)
				 (shift-cuestring-right cuestring n))
				(t
				 (shift-cuestring-left cuestring n))))

	 (rotate-cuestring (cuestring n)
			   (cond ((null n)
				  cuestring)
				 ((zerop n)
				  cuestring)
				 (t 
				  (rotate cuestring (* -1 n)))))
	 
	 (op-and (a b inv-b)
		 (if inv-b (setf b (invert-cuestring b)))
		 (let ((s ""))
		   (dotimes (i (length a))
		     (let ((ca (char a i))
			   (cb (char b i)))
		       (setf s (str+ s (if (and (char= ca #\1)(char= cb #\1)) "1" "0")))))
		   s))

	 (op-or (a b inv-b)
		(if inv-b (setf b (invert-cuestring b)))
		(let ((s ""))
		  (dotimes (i (length a))
		    (let ((ca (char a i))
			  (cb (char b i)))
		      (setf s (str+ s (if (or (char= ca #\1)(char= cb #\1)) "1" "0")))))
		  s))

	 (op-xor (a b inv-b)
		 (if inv-b (setf b (invert-cuestring b)))
		 (let ((s ""))
		   (dotimes (i (length a))
		     (let ((ca (char a i))
			   (cb (char b i)))
		       (setf s (str+ s (if (char= ca cb) "0" "1")))))
		   s))

	 (op-nand (a b inv-b)
		  (if inv-b (setf b (invert-cuestring b)))
		  (let ((s ""))
		    (dotimes (i (length a))
		      (let ((ca (char a i))
			    (cb (char b i)))
			(setf s (str+ s (if (and (char= ca #\1)(char= cb #\1)) "0" "1")))))
		    s))
	 
	 (op-nor (a b inv-b)
		 (if inv-b (setf b (invert-cuestring b)))
		(let ((s ""))
		  (dotimes (i (length a))
		    (let ((ca (char a i))
			  (cb (char b i)))
		      (setf s (str+ s (if (or (char= ca #\1)(char= cb #\1)) "0" "1")))))
		  s))

	 (op-nxor (a b inv-b)
		  (if inv-b (setf b (invert-cuestring b)))
		 (let ((s ""))
		   (dotimes (i (length a))
		     (let ((ca (char a i))
			   (cb (char b i)))
		       (setf s (str+ s (if (char= ca cb) "1" "0")))))
		   s)) )

	(defmethod cuelist ((clist list) &key (form :list) timesig (use-subbeats t))
	  (cond ((eq form :binary)
		 (cuelist->cuestring clist timesig use-subbeats))
		(t clist)))

	(defmethod cuelist ((cuestring string) &key (form :list) timesig (use-subbeats t))
	  (cond ((eq form :binary)
		 (let* ((cstr (compress cuestring))
			(tsig (select-time-signature timesig))
			(event-count (* (bars tsig)(beats tsig)
					(if use-subbeats (subbeats tsig)(tsubbeats tsig)))))
		   (if (notevery #'binary-char-p cstr)
		       (cyco-error "Invalid binary-cuestring" cuestring))
		   (pad-zeros cstr event-count)))
		(t (cuestring->cuelist cuestring timesig use-subbeats))))

	(defmethod duck ((cuelist t)(mask t) &key (invert nil) timesig (use-subbeats t))
	  (let* ((tsig (select-time-signature timesig))
		 (points (cue-points tsig (not use-subbeats)))
		 (msk (or mask (scopies points "0")))
		 (op (if invert :and :and!))
		 (rs (mask-cuelist cuelist msk :op op :timesig tsig :use-subbeats use-subbeats)))
	    rs))
	    
	(defmethod pprint-cuelist ((cuelist list) &key header form timesig (use-subbeats t))
	  (print-header header)
	  (cond ((eq form :binary)
		 (-pprint-cuestring (cuelist cuelist :form :binary :timesig timesig :use-subbeats use-subbeats)
				    timesig use-subbeats))
		(t (-pprint-cuelist cuelist))))

	(defmethod pprint-cuelist ((cuestring string) &key header form timesig (use-subbeats t))
	  (print-header header)
	  (cond ((eq form :binary)
		 (-pprint-cuestring cuestring timesig use-subbeats))
		(t (-pprint-cuelist (cuelist cuestring :timesig timesig :use-subbeats use-subbeats)))))

	(defmethod mask-cuelist ((cuelist t) mask &key op shift rotate timesig (use-subbeats t))
	  (let* ((tsig (select-time-signature timesig))
		 (a (cuelist cuelist :form :binary :timesig tsig :use-subbeats use-subbeats)))
	    (setf a (rotate-cuestring a rotate))
	    (setf a (shift-cuestring  a shift))
	    (let ((b (cuelist mask :form :binary :timesig tsig :use-subbeats use-subbeats)))
	      (cond ((eq op :and)
		     (op-and a b nil))
		    ((eq op :or)
		     (op-or a b nil))
		    ((eq op :xor)
		     (op-xor a b nil))
		    ((eq op :nand)
		     (op-nand a b nil))
		    ((eq op :nor)
		     (op-nor a b nil))
		    ((eq op :nxor)
		     (op-nxor a b nil))
		    ((eq op :and!)
		     (op-and a b t))
		    ((eq op :or!)
		     (op-or a b t))
		    ((eq op :xor!)
		     (op-xor a b t))
		    ((eq op :nand!)
		     (op-nand a b t))
		    ((eq op :nor!)
		     (op-nor a b t))
		    ((eq op :nxor!)
		     (op-nxor a b t))
		    ((eq op :not)
		     (invert-cuestring a))
		    (t a))))) )


(setf (documentation 'duck 'function)
      "Remove cuelist elements which correspond to mask elements.

cuelist - List in 'BAR' format ((bar beat subbeat) ...)
mask    - Mask may take any of following forms:
          A) Cuelist in 'BAR' format.
          B) Binary string, white space is ignored.
             The length of the string is automatically padded 
             with zeros.
          C) An instance of the PART type.
             (Currently STRUMMER is not supported)
:invert - Boolean, it true invert selection.

Let C be the cuelist and M the mask. 
If invert is nil the result is:  C and (not M).
If invert is non-nil the result is: C and M.")
