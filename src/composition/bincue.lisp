;;;; cyco composition binary-cue.lisp
;;;;
;;;; Provides translation between 'binary' cue-list and form expected 
;;;; by the BAR cue function. 
;;;;

(in-package :cyco)

(defclass bincue nil
  ((timesig
    :initform (time-signature)
    :initarg :timesig)
   (use-subbeats
    :initform t
    :initarg :use-subbeats)
   (symbols
    :type list
    :initform '()
    :initarg :symlist)
   (gamut
    :type list
    :initform '())))

(labels ((select-time-signature (arg)
				(cond ((time-signature-p arg)
				       arg)
				      ((and arg (listp arg))
				       (let ((bars (or (car arg) 2))
					     (beats (or (second arg) 4))
					     (sub (or (third arg) 4)))
					 (time-signature :bars bars :beats beats :subbeats sub)))
				      (t
				       (or (and *project* (property *project* :current-section))
					     (let ((frmt "Invalid time-signature argument for BINCUE ~A"))
					       (cyco-warning (sformat frmt arg)
							     "Using default 4/4 time.")
					       (time-signature :bars 8 :beats 4 :subbeats 4))))))

	 (zeros (n)(scopies n #\0))

	 (one+zeros (n)(sformat "1~A" (zeros (1- n))))

	 (trim-white (s)
		     (string-trim '(#\space #\tab #\newline) (->string s)))


	 (process-token (token symlist width)
			(trim-white (or (cdr (assoc token symlist))
					(string-pad-left (->string token) width #\0))))
	 
	 (parse-cuelist (bincue cuelist)
			(let ((acc "")
			      (width (if (slot-value bincue 'use-subbeats)
					 (subbeats (slot-value bincue 'timesig))
				       (tsubbeats (slot-value bincue 'timesig))))
			      (symlist (slot-value bincue 'symbols)))
			  (dolist (token cuelist)
			    (setf acc (sformat "~A~A" acc (process-token token symlist width))))
			  acc)) )

	(defun bincue (&key (symbols '())(timesig nil)(use-subbeats t))
	  (let* ((tsig (select-time-signature timesig))
		 (beats (beats tsig))
		 (subs (if use-subbeats
			   (subbeats tsig)
			 (tsubbeats tsig)))
		 (q subs)
		 (e (if (oddp subs)
			(truncate (1+ (/ q 2)))
		      (/ q 2)))
		 (h (* 2 subs))
		 (w (* 4 subs))
		 (acc (list (cons 's "1")
			    (cons 'e (one+zeros e))
			    (cons 'q (one+zeros q))
			    (cons 'h (one+zeros h))
			    (cons 'w (one+zeros w))
			    ;; rest 
			    (cons 'sr "0")
			    (cons 'er (zeros e))
			    (cons 'qr (zeros q))
			    (cons 'hr (zeros h))
			    (cons 'wr (zeros w))
			    (cons 'bar (zeros (* subs beats))))))
	    (dolist (us symbols)
	      (let ((sym (car us))
		    (val (remove-if #'(lambda (c)
					(member c '(#\space #\tab #\newline) :test #'char=))
				    (->string (cdr us)))))
		(push (cons sym val) acc)))
	    (dotimes (i (bars tsig))
	      (let ((sym (->symbol (sformat "~A-BAR" (1+ i))))
		    (exp (zeros (* (1+ i) subs beats))))
		(push (cons sym exp) acc)))
	    (let ((bincue (make-instance 'bincue
					 :timesig tsig
					 :use-subbeats use-subbeats
					 :symlist acc)))
	      (setf (slot-value bincue 'gamut)(->vector (cue-gamut tsig (not use-subbeats))))
	      bincue))) 

	(defmethod bincue-translate ((bincue bincue)(cuelist list))
	  (let ((blist (parse-cuelist bincue cuelist))
		(gamut (slot-value bincue 'gamut))
		(acc '()))
	    (dotimes (index (length blist))
	      (if (>= index (length gamut))
		  (cyco-error (sformat "BINCUE cuelist index out of bounds: ~A~%" index)))
	      (let ((c (char blist index)))
		(if (char= c #\1)
		    (push (aref gamut index) acc))))
	    (reverse acc)))


	(defmethod ?bincue ((bincue bincue))
	  (let* ((tsig (slot-value bincue 'timesig))
		 (use-subs (slot-value bincue 'use-subbeats))
		 (width (if use-subs
			    (subbeats tsig)
			  (tsubbeats tsig))))
	    (dolist (c (slot-value bincue 'symbols))
	      (let ((sym (car c))
		    (val (->string (cdr c))))
		(format t "~12A ->" sym)
		(loop for i from 0 below (length val) do
		      (if (zerop (rem i width))
			  (format t " ~A" (char val i))
			(format t "~A" (char val i))))
		(format t "~%"))))) )

(setf (documentation 'bincue 'function)
      (substitute-if #\" #'(lambda (c)(char= c #\~))
      "Creates instance of BINCUE, a binary cue-list translator.

A BINCUE object translates a cue-list in 'binary' form to the format used
by the BAR cue-function.  A binary cue-list is a list of binary numbers and
special symbols.  Assuming the common 4/4 time with 4 subbeats per beat,
each bit position corresponds to successive 16th notes.  The pattern 
(1000 1000 1000 1000) produces an event on each quarter notes.  Embedded 
spaces are ignored so the above could also be expressed as 1000100010001000.  
A few special symbols are provided, the same 4-beat pattern could be
expressed by (q q q q).  Spaces are required when using symbols.  Use the
?BINCUE method to display a list of symbols.  Finally the BINCUE-TRANSLATE
method is used to translate the binary cue-list into the form required by
the BAR cue-function.

:symbols - Association list of user defined cue patterns.  The list contents 
           are appended to the set of common symbols.

           :symbols '((CLAVE . ~1000 0010 0010 0000 0000 1000 1000 0000~))

           Adds CLAVE to the symbols list.  When the symbol CLAVE is
           encountered in the cue-list it is converted to the familiar
           2-bar ~clave~ rhythm.  Spaces are for readability only and are
           completely ignored. 

:timesig - Sets time-signature parameters.  The BINCUE time-signature must
           match the part (usually a QBALL or XBALL) for which it is used.
           The timesig argument may take the following forms.

           1) nil - The default, uses the current-section of *project*.
           2) An actual time-signature object.
           3) List of form (bar beat sub) creates a new time-signature of
              indicated bar, beat and subbeat counts.  All values are
              optional with defaults 2-bars, 4-beats and 4-subbeats. 

:use-subbeats - Boolean.  Normally each cue-list bit corresponds to a
                single time-signature subbeat.  Set use-subbeats to NIL to
                use tsubbeats instead."))


(setf (documentation 'bincue-translate 'function)
      "BINCUE method which performs the actual binary cue-list conversion.")

(setf (documentation '?bincue 'function)
      "Displays list of BINCUE cue-list symbols.")
