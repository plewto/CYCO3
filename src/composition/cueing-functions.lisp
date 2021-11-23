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

TK - Tick offset, may be positive or negative integer, default 0.")

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
	   
	   (tick-value (time-signature time-specification token)
		       (or (and (not token) 0)
			   (and (integerp token)
				(* (tick-duration time-signature) token))
			   (warnfn time-specification))) )
    
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

