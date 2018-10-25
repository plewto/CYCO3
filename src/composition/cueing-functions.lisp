;;;; CYCO3 src/composition/cueing-functions
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
			 (cyco-cue-warning cue-fn args)))
       )

  ;; Only takes int arguments
  ;; Performs range test
 
  (defun bar (time-signature args)
    (let* ((v (->vector (fill-list args '(1 1 1 0))))
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
			  

		 
    



