;;;; CYCO3 src/composition/parts/cpart-render
;;;;
          ;; Generates single bend event for each channel-index.
(labels ((create-bend-point
	  (time cindex-list value blur)
	  (let* ((value~ (approximate value :scale blur :min -1 :max +1))
		 (data (bend->midi-data value~))
		 (lsb (aref data 0))
		 (msb (aref data 1))
		 (bcc '()))
	    (dolist (ci cindex-list)
	      (push (cons time (midi-pitch-bend ci lsb msb)) bcc))
	    bcc))

	 ;; Generates MIDI controller event for each channel-index.
	 (create-controller-point
	  (time cindex-list controller-number value blur)
	  (let* ((value~ (approximate value :scale blur :min 0 :max +1))
		 (data (norm->midi-data value~))
		 (bcc '()))
	    (dolist (ci cindex-list)
	      (push (cons time (midi-control-change ci controller-number data)) bcc))
	    bcc))

	 ;; Generates MIDI channel pressure event for each channel index.
	 (create-pressure-point
	  (time cindex-list value blur)
	  (let* ((value~ (approximate value :scale blur :min 0 :max +1))
		 (data (norm->midi-data value~))
		 (bcc '()))
	    (dolist (ci cindex-list)
	      (push (cons time (midi-channel-pressure ci data)) bcc))
	    bcc))

	 ;; For each channel-index, generates a single MIDI controller,
	 ;; channel-pressure or bend event at time2.
	 ;; Returns nested list. 
	 (create-point
	  (time cindex-list event-type value blur)
	  (cond ((and (numberp event-type) (<= 0 event-type) (< event-type 128))
	 	 (create-controller-point time cindex-list event-type value blur))
	 	((eq event-type 'touch)
	 	 (create-pressure-point time cindex-list value blur))
	 	((eq event-type 'bend)
		 (create-bend-point time cindex-list value blur))
		(t (cyco-warning
		    (sformat "Invalid CPART event-type ~A" event-type))
		   nil))) 

	 (create-line
	  (state offset cindex-list)
	  (let* ((time1 (float (+ offset (cpart-state-time1 state))))
		 (time2 (+ offset (cpart-state-time2 state)))
		 (steps (cpart-state-steps state))
		 (time-increment (/ (abs (- time2 time1)) (1- steps)))
		 (value1 (float (cpart-state-value1 state)))
		 (value2 (cpart-state-value2 state))
		 (value-increment (/ (- value2 value1) (1- steps)))
		 (degree (cpart-state-cycles state))
		 (time time1)
		 (value value1)
		 (bcc '()))
	    (dotimes (i steps)
	      (setf bcc (append (create-point time cindex-list
					      (cpart-state-event-type state)
					      (expt value degree)
					      (cpart-state-blur state))
				bcc))
	      (setf time (+ time time-increment))
	      (setf value (+ value value-increment)))
	    bcc))

	 ;; Creates sawtooth "scanner" wave with peaks at y1, y2.
	 ;; Slope is positive for y1<y2, and negative for y2<y1.
	 ;; Times, step and cycle count derived from state.
	 ;; Returns nested list  ((time . value)(time . value) ...)
	 ;; 
	 (scanner
	  (state offset y1 y2)	
	  (let* ((time1 (float (+ offset (cpart-state-time1 state))))
	 	 (time2 (+ offset (cpart-state-time2 state)))
	 	 (cycles (max 0 (cpart-state-cycles state)))
	 	 (steps (/ (cpart-state-steps state) cycles))
	 	 (time-increment (/ (abs (- time2 time1)) (1- steps)))
	 	 (vdiff (- y2 y1))
	 	 (value-increment (/ vdiff (1- steps)))
	 	 (time time1)
	 	 (value y1)
	 	 (bcc '()))
	    (dotimes (i (cpart-state-steps state))
	      (push (cons time value) bcc)
	      (setf time (+ time time-increment))
	      (setf value (+ value value-increment))
	      (if (< y1 y2)
		  (if (> value y2)(setf value y1))
		(if (< value y2)(setf value y1))))
	    (reverse bcc)))

	 (create-sawtooth
	  (state offset cindex-list)
	  (let* ((y1 (cpart-state-value1 state))
		 (y2 (cpart-state-value2 state))
		 (points (scanner state offset y1 y2))
		 (event-type (cpart-state-event-type state))
		 (blur (cpart-state-blur state))
		 (bcc '()))
	    (dolist (p points)
	      (setf bcc (append bcc (create-point (car p) cindex-list
						  event-type (cdr p) blur))))
	    bcc))

	 (create-triangle
	  (state offset cindex-list)
	  (let* ((y1 (cpart-state-value1 state))
		 (y2 (cpart-state-value2 state))
		 (saw (scanner state offset -1 1))
		 (points (let* ((a (float (- y1 y2))) ; wave-shape scanner sawtooth
				(b (- y1 (/ a 2)))    ; to triangle.
				(tri '()))
			   (dolist (p saw)
			     (let* ((time (car p))
				    (s (- (abs (cdr p)) 1/2))
				    (v (+ (* a s) b)))
			       (push (cons time v) tri)))
			   tri))
		 (event-type (cpart-state-event-type state))
		 (blur (cpart-state-blur state))
		 (bcc '()))
	    (dolist (p points)
	      (setf bcc (append bcc (create-point (car p) cindex-list event-type (cdr p) blur))))
	    bcc))

	 (create-random-values
	  (state offset cindex-list)
	  (let* ((time1 (float (+ offset (cpart-state-time1 state))))
		 (time2 (+ offset (cpart-state-time2 state)))
		 (steps (cpart-state-steps state))
		 (time-increment (/ (abs (- time2 time1)) (1- steps)))
		 (v1 (cpart-state-value1 state))
		 (v2 (cpart-state-value2 state))
		 (diff (float (abs (- v2 v1))))
		 (floor (min v1 v2))
		 (event-type (cpart-state-event-type state))
		 (time time1)
		 (bcc '()))
	    (dotimes (i steps)
	      (let ((value (+ floor (random diff))))
		(setf bcc (append bcc (create-point time cindex-list event-type value 0.0)))
		(setf time (+ time time-increment))))
	    bcc))
	    
	 (create-sin1
	  (state offset cindex-list)
	  (let* ((v1 (cpart-state-value1 state))
		 (v2 (cpart-state-value2 state))
		 (saw (scanner state offset 0 (* 2 pi)))
		 (a (/ (- v2 v1) 2.0))
		 (b (+ v1 a))
		 (event-type (cpart-state-event-type state))
		 (blur (cpart-state-blur state))
		 (bcc '()))
	    (dolist (p saw)
	      (let ((s (+ (* (sin (cdr p)) a) b)))
		(setf bcc (append bcc (create-point (car p) cindex-list event-type s blur)))))
	    bcc))

	 (create-cos1
	  (state offset cindex-list)
	  (let* ((v1 (cpart-state-value1 state))
		 (v2 (cpart-state-value2 state))
		 (saw (scanner state offset 0 (* 2 pi)))
		 (a (/ (- v2 v1) 2.0))
		 (b (+ v1 a))
		 (event-type (cpart-state-event-type state))
		 (blur (cpart-state-blur state))
		 (bcc '()))
	    (dolist (p saw)
	      (let ((s (+ (* (cos (cdr p)) a) b)))
		(setf bcc (append bcc (create-point (car p) cindex-list event-type s blur)))))
	  bcc))

	 (create-sin2
	  (state offset cindex-list)
	  (let* ((y1 (cpart-state-value1 state))
		 (y2 (cpart-state-value2 state))
		 (saw (scanner state offset 0 (* 2 pi)))
		 (a (/ (- y1 y2) 3.0))
		 (b (- y1 (* 3/2 a)))
		 (event-type (cpart-state-event-type state))
		 (blur (cpart-state-blur state))
		 (bcc '()))
	    (dolist (p saw)
	      (let* ((x (cdr p))
		     (s1 (sin x))
		     (s2 (* 1/2 (sin (* 2 x))))
		     (s (+ b (* a (+ s1 s2))))
		     (pnt (create-point (car p) cindex-list event-type s blur)))
		(setf bcc (append bcc pnt))))
	    bcc))

	 (create-cos2
	  (state offset cindex-list)
	  (let* ((y1 (cpart-state-value1 state))
		 (y2 (cpart-state-value2 state))
		 (saw (scanner state offset 0 (* 2 pi)))
		 (a (/ (- y1 y2) 3.0))
		 (b (- y1 (* 3/2 a)))
		 (event-type (cpart-state-event-type state))
		 (blur (cpart-state-blur state))
		 (bcc '()))
	    (dolist (p saw)
	      (let* ((x (cdr p))
		     (s1 (cos x))
		     (s2 (* 1/2 (cos (* 2 x))))
		     (s (+ b (* a (+ s1 s2))))
		     (pnt (create-point (car p) cindex-list event-type s blur)))
		(setf bcc (append bcc pnt))))
	    bcc))

	 (create-lin+cos
	  (state offset cindex-list)
	  (let* ((time1 (float (+ offset (cpart-state-time1 state))))
		 (time2 (+ offset (cpart-state-time2 state)))
		 (steps (cpart-state-steps state))
		 (time-increment (/ (abs (- time2 time1))(1- steps)))
		 (y1 (float (cpart-state-value1 state)))
		 (y2 (cpart-state-value2 state))
		 (y-increment (/ (- y2 y1)(1- steps)))
		 (cosx1 (* 2 pi (cpart-state-cycles state)))
		 (cosx-increment (/ cosx1 steps))
		 (cos-amp 1/16)
		 (cos-scale (/ cos-amp 2))
		 (cos-offset (- cos-amp cos-scale))
		 (event-type (cpart-state-event-type state))
		 (blur (cpart-state-blur state))
		 (time time1)
		 (y y1)
		 (cosx cosx1)
		 (bcc '()))
	    (dotimes (i steps)
	      (let* ((c (+ (* cos-amp (cos cosx)) cos-offset))
		     (q (+ c y)))
		(setf bcc (append bcc (create-point time cindex-list event-type q blur)))
		(setf time (+ time time-increment))
		(setf y (+ y y-increment))
		(setf cosx (+ cosx cosx-increment))))
	    bcc)) )

  (defmethod render-once ((cpart cpart) &key (offset 0))
    (let* ((acc '())
	   (cindex-list (let* ((ipat (property cpart :instruments))
			       (ilist (->list (next ipat :all))))
			  (mapcar #'channel-index ilist))))
      (dolist (state (cpart-events cpart))
	(let* ((time2 (+ offset (cpart-state-time2 state)))
	       (value1 (cpart-state-value1 state))
	       (curve (cpart-state-curve-type state))
	       (event-type (cpart-state-event-type state))
	       (blur (or (cpart-state-blur state) 0.0)))
	  (setf acc (cond ((eq curve 'point)
			   (append acc
				   (create-point time2 cindex-list
						 event-type value1 blur)))
			  ((eq curve 'line)
			   (append acc (create-line state offset cindex-list)))
			  ((eq curve 'saw)
			   (append acc (create-sawtooth state offset cindex-list)))
			  ((eq curve 'tri)
			   (append acc (create-triangle state offset cindex-list)))
			  ((eq curve 'random)
			   (append acc (create-random-values state offset cindex-list)))
			  ((eq curve 'sin)
			   (append acc (create-sin1 state offset cindex-list)))
			  ((eq curve 'cos)
			   (append acc (create-cos1 state offset cindex-list)))
			  ((eq curve 'sin2)
			   (append acc (create-sin2 state offset cindex-list)))
			  ((eq curve 'cos2)
			   (append acc (create-cos2 state offset cindex-list)))
			  ((eq curve 'lin+cos)
			   (append acc (create-lin+cos state offset cindex-list)))))))
      (dolist (c (reverse (children cpart)))
	(setf acc (append acc (render-once c :offset offset))))
      (sort-midi-events acc))) )

(defmethod render-n ((part cpart)(n integer) &key (offset 0.0))
  (let ((period (phrase-duration part))
	(template (render-once part))
	(acc '()))
    (dotimes (i (if (property part :render-once) 1 n))
      (dolist (evn template)
	(let ((reltime (car evn))
	      (msg (cdr evn)))
	  (push (cons (+ offset (* i period) reltime) msg) acc))))
    (sort-midi-events acc)))

