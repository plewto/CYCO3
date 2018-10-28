;;;; CYCO3 src/composition/cueing-functions
;;;; Cueing functions are closely aligned with time-signatures.
;;;; The convert a user specified time-point to actual time
;;;; using the current time-signature.  Usually the time point
;;;; is some offset from the start of a Section or Part.
;;;;


(flet ((expect-int (cue-fn value max args)
		   (or (and (integerp value)
			    (plusp value)
			    (<= value max))
		       (progn
			 (cyco-cue-warning cue-fn args)
			 nil)))
       (expect-tick (cue-fn value args)
		    (or (integerp value)
			(cyco-cue-warning cue-fn args)))
       (expect-rational (cue-fn value args)
			(or (and (<= 0 value)(< value 1))
			    (cyco-cue-warning cue-fn args)))
       (expect-float (cue-fn value args)
		     (or (and (<= 0 value)(< value 1))
			 (cyco-cue-warning cue-fn args))) )

  (defun bar (time-signature args)
    "Bar is the default cueing function, it takes a time-signature and a 
list of timing values.   The argument format is 

       (bar beat sub-beat ticks)

All values are optional, and with exception of ticks, default to 1.

As example to specify the first beat of bar 1 any of the following args
list may be used:   () (1) (1 1) (1 1 1) (1 1 1 0)

The accepted range of values is dependent on the the time-signature
For a common case of an 8-bar phrase of 4/4 time with 4 16th notes per 
quarter note, the bars slot has a range of 1..8, the beats and subbeats 
slot have ranger 1..4 and the ticks slot has a range 0..+TICKS-PER-BEAT+

The user is free to define custom cueing functions to meet their 
specific needs."

    (let* ((v (->vector (fill-list (->list args) '(1 1 1 0))))
	   (bar (aref v 0))
	   (beat (aref v 1))
	   (sub (aref v 2))
	   (tk (aref v 3)))
      (expect-int 'bar bar (bars time-signature) args)
      (expect-int 'bar beat (beats time-signature) args)
      (expect-int 'bar sub (subbeats time-signature) args)
      (expect-tick 'bar tk args)
      (+ (* (1- bar)(bar-duration time-signature))
	 (* (1- beat)(beat-duration time-signature))
	 (* (1- sub)(subbeat-duration time-signature))
	 (* tk (tick-duration time-signature)))))


  ;; More relaxed, requires int for bar and beat
  ;; allows int, float or rational for sub
  ;; (defun rbar (time-signature &optional (bar 1)(beat 1)(sub 1)(tick 0))
  ;;   (let* ((args (list bar beat sub tick))
  ;; 	   (subpos (cond ((integerp sub)
  ;; 			  (expect-int 'rbar sub (subbeats time-signature) args)
  ;; 			  (1- sub))
  ;; 			 ((rationalp sub)
  ;; 			  (expect-rational 'rbar sub args)
  ;; 			  sub)
  ;; 			 ((floatp sub)
  ;; 			  (expect-float 'rbar sub args)
  ;; 			  sub)
  ;; 			 (t
  ;; 			  (cyco-cue-warning 'rbar args)
  ;; 			  0))))
  ;;     (expect-int 'bar bar (bars time-signature) args)
  ;;     (expect-int 'bar beat (beats time-signature) args)
  ;;     (expect-tick 'bar tick args)
  ;;     (+ (* (1- bar)(bar-duration time-signature))
  ;; 	 (* (1- beat)(beat-duration time-signature))
  ;; 	 (* subpos (subbeat-duration time-signature))
  ;; 	 (* tick (tick-duration time-signature)))))

  ) 
			  

		 
    



