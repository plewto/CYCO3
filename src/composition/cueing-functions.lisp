;;;; CYCO composition  cueing-functions.lisp
;;;;
;;;; Cueing functions convert a user specified time-point to real time
;;;; using the current time-signature.  Usually the time point is some
;;;; offset from the start of a Section or Part.
;;;;
;;;; The general function form is
;;;;
;;;;       (lambda time-signature time-specification) --> float
;;;;
;;;; The time-specification format is intentionally undefined.  Implementing
;;;; functions are free to use whatever format meets their specific needs.


(in-package :cyco)

;; The Default cueing function BAR
;;

(let ((bar-docstring
"BAR is the default cueing function.
time specification in the context of the time-signature.
The specification has the form (BR BT SB TK) where all elements are optional. 

BR - Bar number, integer 1,2,3,... <= (bars time-signature), default 1

BT - Beat number, integer 1,2,3,... <= (beats time-signature), default 1
     Use a 'T' prefix for triplet version of the beat unit.
     T1,T2,T3,... <= (tbeats time-signature)

SB - Sub beat within beat, 1,2,3,... <= (subbeats time-signature), default 1
     Use 'T' prefix for triplet version of sub-bet
     T1,T2,T3,... <= (tsubbeats time-signature)

TK - Tick offset, may be signed integer (absolute) or signed float (relative). 
     For integer +/- n, n tick-duration added/subtracted from time.
     For float +/- f, where 0 <= |f| <= 1, a fraction of a single subbeat is 
     added or subtracted from the time. 

     For example if tk is 0.333, the result is increased by a third of a subbeat.")

      (tbar-docstring
       "TBAR is an alternate cueing function which is easier to use for triplets.
TBAR behaves exactly like BAR unless the first element of the time-cue is the
symbol 't, in which case it switches to eighth-note triplet mode.

      (T BAR-NUMBER TRIPLET-NUMBER)

For a 4/4 bar  1 <= triplet-number <= 12") )
  
  (labels ((warnfn (args)
		   (cyco-cue-warning 'BAR args)
		   0.0)
	   
	   (bar-value (time-signature time-specification token)
		      (or (and (integerp token)(plusp token)(<= token (bars time-signature))
			       (* (bar-duration time-signature)(1- token)))
			  (warnfn time-specification)))
	   
	   (is-triplet-p (token-string)
			 (char= #\T (char token-string 0)))
	   
	   (beat-triplet-value (time-signature time-specification stoken)
			       (let ((n (parse-integer (subseq stoken 1))))
				 (or (and (plusp n)(<= n (tbeats time-signature))
					  (* (tbeat-duration time-signature)(1- n)))
				     (warnfn time-specification))))
	   
	   (beat-normal-value (time-signature time-specification token)
			      (or (and (integerp token)(plusp token)(<= token (beats time-signature))
				       (* (beat-duration time-signature)(1- token)))
				  (warnfn time-specification)))
	   
	   (beat-value (time-signature time-specification token)
		       (let ((stoken (string-upcase (->string token))))
			 (if (is-triplet-p stoken)
			     (beat-triplet-value time-signature time-specification stoken)
			   (beat-normal-value time-signature time-specification token))))
	   
	   (subbeat-triplet-value (time-signature time-specification stoken)
				  (let ((n (parse-integer (subseq stoken 1))))
				    (or (and (plusp n)(<= n (tsubbeats time-signature))
					     (* (tsubbeat-duration time-signature)(1- n)))
					(warnfn time-specification))))
	   
	   (subbeat-normal-value (time-signature time-specification token)
				 (or (and (integerp token)(plusp token)(<= token (subbeats time-signature))
					  (* (subbeat-duration time-signature)(1- token)))
				     (warnfn time-specification)))
	   
	   (subbeat-value (time-signature time-specification token)
			  (let ((stoken (string-upcase (->string token))))
			    (if (is-triplet-p stoken)
				(subbeat-triplet-value time-signature time-specification stoken)
			      (subbeat-normal-value time-signature time-specification token))))
	   
	   (tick-int-value (time-signature token)
			   (* (tick-duration time-signature) token))

	   (tick-float-value (time-signature token)
			     (let* ((ratio (- token (truncate token)))
				    (ticks (round (* ratio (ticks-per-subbeat time-signature)))))
			       (* (tick-duration time-signature) ticks)))
	   
	   (tick-value (time-signature time-specification token)
		       (cond ((not token) 0)
			     ((integerp token)
			      (tick-int-value time-signature token))
			     ((floatp token)
			      (tick-float-value time-signature token))
			     (t (warnfn time-specification)))) )
    
    (defun bar (time-signature time-specification)
      bar-docstring
      (let* ((v (->vector (fill-list (->list time-specification) '(1 1 1 0))))
	     (br (bar-value time-signature time-specification (aref v 0)))
	     (bt (beat-value time-signature time-specification (aref v 1)))
	     (sb (subbeat-value time-signature time-specification (aref v 2)))
	     (tk (tick-value time-signature time-specification (aref v 3))) )
	(float (+ br bt sb tk))))
    
    (defun tbar (time-signature time-cue)
      tbar-docstring
      (if (eq (car time-cue) 't)
	  (let ((bar-number (or (second time-cue) 1))
		(triplet-number (or (third time-cue) 1)))
	    (+ (* (1- bar-number) (bar-duration time-signature))
	       (* 0.5 (1- triplet-number) (tbeat-duration time-signature))))
	(bar time-signature time-cue))) ))


(defun cue-n (division)
  #'(lambda (time-signature time-cue)
      (let* ((br (max 0 (1- (or (car time-cue) 1))))
	     (n (max 0 (or (second time-cue) 0)))
	     (jog (or (third time-cue) 0))
	     (brdur (bar-duration time-signature))
	     (ndur (/ brdur division))
	     (jdur (/ ndur 16)))
	(+ (* br brdur)
	   (* n ndur)
	   (* jog jdur)))))
	
		 
(setf (documentation 'cue-n 'function)
      "CUE-N returns a cuing-function which evenly divides a bar into division sections.

The time-cue argument for the resulting function has the form

    (bar n [jog])

bar - bar-number 1, 2, 3, ...
n   - number of division units after the start of the bar  0, 1, 2, ...
      The n argument may be greater then the division to set a time after 
      the current bar.
jog - optional finer grained offset, shifts time by jog/16 of the division 
      unit.  jog may be negative or positive.


Example 

     (param tsig (time-signature :tempo 60 :bars 4 :beats 4))
     (param fn (cue-n 4))

     (funcall fn tsig '(1))     --> 0.00    start bar 1
     (funcall fn tsig '(1 0))   --> 0.00    beat 1, bar 1
     (funcall fn tsig '(1 1))   --> 1.00    beat 2, bar 1
     (funcall fn tsig '(1 1 4)) --> 1.25    first 16th note after beat 1
     (funcall fn tsig '(nil 4)) --> 4.00    start of bar 2")


(labels ((ebar (n n-beats n-subbeats)
	       (let ((q (* n-beats n-subbeats)))
	 	 (1+ (truncate (/ n q)))))
	 (ebeat (n n-beats n-subbeats)
	 	(let ((q (* n-beats n-subbeats)))
	 	  (1+ (truncate (/ (rem n q) n-subbeats)))))
	 (esub (n subbeats)
	       (cnth n subbeats)))

	(defun enumerate-cue (n &key (n-beats 4)(subbeats '(1 2 3 4)))
	  "Converts integer index to BAR function cue specification.
n - index
:n-beats - number of beats per bar
:subbeats - list of possible sub beats"
	  (let ((n-subbeats (length subbeats)))
	    (list (ebar n n-beats n-subbeats)
		  (ebeat n n-beats n-subbeats)
		  (esub n subbeats)))))


(labels ((convert-spec (arg)
		       (cond ((numberp arg)
			      (range 1 (1+ arg)))
			     (t (->list arg)))))

	(defun gencue (&key (bars 4)(beats 4)(subbeats '(1 3))
			    (filter #'(lambda (br bt sb)
					(declare (ignore br bt sb))
					nil)))
						       
	  "Generates BAR function cue-list by iterating bar, beat and subbeats
All arguments time arguments may either be a positive integer or list of possible values.
Integers n are converted to list  (1 2 3 ... n)

:bars - possible bar-numbers
:beats - possible beat-numbers
:subbeats - possible subbeat numbers
:filter - function suppresses specific combinations of bar, beat and subbeat.
filter takes three arguments and returns bool, if the result is true, the combination
is suppressed.

returns nested list."
	  (let ((acc '()))
	    (dolist (br (convert-spec bars))
	      (dolist (bt (convert-spec beats))
		(dolist (sb (convert-spec subbeats))
		  (if (not (funcall filter br bt sb))
		      (push (list br bt sb) acc)))))
	    (reverse acc))))


(defun pprint-cuelist (cue-list &optional header)
  (let ((current (caar cue-list)))
    (if header
	(format t "~A CUE-LIST:~%" header))
    (dolist (q cue-list)
      (if (not (eq (car q) current))
	  (progn
	    (format t "~%")
	    (setf current (car q))))
      (format t "~A " q))
    (format t "~%")))
