;;;; CYCO
;;;;
;;;; Cueing functions convert a user specified time-point to actual time
;;;; using the current time-signature.  Usually the time point is some
;;;; offset from the start of a Section or Part.
;;;;


;; The Default cueing function BAR
;;
(labels ((warnfn
	  (args)
	  (cyco-cue-warning 'BAR args)
	  0.0)

	 (bar-value
	  (tsig args token)
	  (or (and (integerp token)(plusp token)(<= token (bars tsig))
	  	   (* (bar-duration tsig)(1- token)))
	      (warnfn args)))

	 (is-triplet-p
	  (stoken)  ;; stoken <-- string
	  (char= #\T (char stoken 0)))

	 (beat-t-value
	  (tsig args stoken)
	  (let ((n (parse-integer (subseq stoken 1))))
	    (or (and (plusp n)(<= n (tbeats tsig))
		     (* (tbeat-duration tsig)(1- n)))
		(warnfn args))))

	 (beat-normal-value
	  (tsig args token)
	  (or (and (integerp token)(plusp token)(<= token (beats tsig))
		   (* (beat-duration tsig)(1- token)))
	      (warnfn args)))

	 (beat-value
	  (tsig args token)
	  (let ((stoken (string-upcase (->string token))))
	    (if (is-triplet-p stoken)
		(beat-t-value tsig args stoken)
	      (beat-normal-value tsig args token))))

	 (subbeat-t-value
	  (tsig args stoken)
	  (let ((n (parse-integer (subseq stoken 1))))
	    (or (and (plusp n)(<= n (tsubbeats tsig))
		     (* (tsubbeat-duration tsig)(1- n)))
		(warnfn args))))

	 (subbeat-normal-value
	  (tsig args token)
	  (or (and (integerp token)(plusp token)(<= token (subbeats tsig))
		   (* (subbeat-duration tsig)(1- token)))
	      (warnfn args)))

	 (subbeat-value
	  (tsig args token)
	  (let ((stoken (string-upcase (->string token))))
	    (if (is-triplet-p stoken)
		(subbeat-t-value tsig args stoken)
	      (subbeat-normal-value tsig args token))))

	 (tick-value
	  (tsig args token)
	  (or (and (not token) 0)
	      (and (integerp token)
		   (* (tick-duration tsig) token))
	      (warnfn args))) )

  (defun bar (time-signature tspec)
    "BAR is the default cueing function.
tspec is a time specification in the context of the time-signature.
The specification has the form (BR BT SB TK) where all elements are optional. 

BR - Bar number, integer 1,2,3,... <= (bars time-signature), default 1

BT - Beat number, integer 1,2,3,... <= (beats time-signature), default 1
     Use a 'T' prefix for triplet version of the beat unit.
     T1,T2,T3,... <= (tbeats time-signature)

SB - Sub beat within beat, 1,2,3,... <= (subbeats time-signature), default 1
     Use 'T' prefix for triplet version of sub-bet
     T1,T2,T3,... <= (tsubbeats time-signature)

TK - Tick offset, may be positive or negative integer, default 0."
    (let* ((v (->vector (fill-list (->list tspec) '(1 1 1 0))))
  	   (br (bar-value time-signature tspec (aref v 0)))
  	   (bt (beat-value time-signature tspec (aref v 1)))
  	   (sb (subbeat-value time-signature tspec (aref v 2)))
  	   (tk (tick-value time-signature tspec (aref v 3))) )
      (float (+ br bt sb tk))))

  (defun float-bar (time-signature tspec)
    "FLOAT-BAR is an alternate cuing function which allows fractional 
beat parameters  (BR BT)
BR - Bar number, integer >= 1
BT - Beat number, float/rational >= 1"
    (let* ((v (->vector (fill-list (->list tspec) '(1 1))))
	   (br (bar-value time-signature tspec (aref v 0)))
	   (bt (* (beat-duration time-signature)(1- (aref v 1)))))
      (float (+ br bt)))) ) 



