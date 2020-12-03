
(in-package :cyco-part)

(global *strummer-render-trace* nil)

(labels ((render-bend 
	  (time state channel-index)
	  (let ((normalized-bend (strummer-state-bend state)))
	    (if normalized-bend
		(let ((data (bend->midi-data normalized-bend)))
		  (cons time (midi-pitch-bend channel-index (aref data 0)(aref data 1))))
	      nil)))
    	 
	 (render-control-change 
	  (time state channel-index)
	  (let ((controller-number (strummer-state-controller-number state))
		(normalized-value (strummer-state-controller-value state)))
	    (if (and controller-number normalized-value)
		(let ((midi-data (norm->midi-data normalized-value)))
		  (cons time (midi-control-change channel-index controller-number midi-data)))
	      nil)))
	 
	 (render-program-change 
	  (time state instrument)
	  (let ((program (strummer-state-program-number state))
		(bank (strummer-state-program-bank state)))
	    (if program
		(let ((pmap (program-map instrument)))
		  (funcall pmap time :bank bank :program program))
	      nil)))

	 (resolve-chord 
	  (strummer state instrument)
	  (dismiss strummer state instrument)
	  '(dummy not list))

	 (purge-rest 
	  (note-list)
	  (remove-if #'rest-p note-list))

	 (invert-chord
	  (note-list degree octave)
	  (let ((result (chord-inversion (reverse note-list) degree :add-octave octave)))
	    (reverse result)))
	 
	 (prepare-note-list 
	  (strummer state root-key)
	  (let* ((chord-model (property strummer :chord-model))
		 (degree (strummer-state-chord-inversion state))
		 (octave (strummer-state-chord-octave state))
		 (direction (next-1 (strummer-state-strum-direction state)))
		 (chord-type (strummer-state-chord-type state))
		 (note-list (remove-if #'rest-p (if (listp chord-type)
						    chord-type
						  (chord-template chord-model chord-type root-key)))))
	    (setf note-list (if (absolute-chords-p chord-model)
				note-list
			      (mapcar #'(lambda (q)(+ q root-key)) note-list)))
	    (setf note-list (cond ((eq direction :up)
				   note-list)
				  ((eq direction :dice)
				   (if (< (random 1000) 500)
				       (reverse note-list)
				     note-list))
				  ((eq direction :random)
				   (permute note-list))
				  (t (reverse note-list))))
	    (invert-chord note-list degree octave)))
	 
	 (prepare-note-start-times 
	  (state note-count)
	  (let ((time (strummer-state-time state))
		(delay (strummer-state-strum-delay state))
		(acceleration (strummer-state-strum-acceleration state))
		(acc '()))
	    (dotimes (i note-count)
	      (push time acc)
	      (setf time (+ time delay))
	      (setf delay (* delay acceleration)))
	    acc))

	 (prepare-note-end-times 
	  (state start-times instrument)
	  (let ((acc '())
		(duration (funcall (articulation-map instrument)
				   (strummer-state-articulation state))))
	    (dolist (start start-times)
	      (push (+ start duration) acc))
	    (if (strummer-state-strum-end-together state)
		(copies (length start-times)(apply #'max acc))
	      (reverse acc))))

	 (prepare-note-velocities 
	  (state note-count root-velocity)
	  (let ((scale (strummer-state-strum-amp-scale state))
		(velocity (float root-velocity))
		(acc '()))
	    (dotimes (i note-count)
	      (push (truncate (limit velocity 1 127)) acc)
	      (setf velocity (* velocity scale)))
	    acc))
	 
	 (strum-chord 
	  (strummer state instrument root-velocity)
	  (if (not (strummer-state-key state))
	      (return-from strum-chord nil))
	  (let ((root-key (funcall (keynumber-map instrument)(strummer-state-key state))))
	    (if (not (rest-p root-key))
		(let* ((note-list (prepare-note-list strummer state root-key))
		       (note-count (length note-list))
		       (start-times (prepare-note-start-times state note-count))
		       (end-times (prepare-note-end-times state start-times instrument))
		       (velocities (prepare-note-velocities state note-count root-velocity))
		       (channel-index (channel-index instrument))
		       (midi-events '()))
		  (dotimes (i note-count)
		    (push (cons (nth i start-times)(midi-note-on channel-index (nth i note-list)(nth i velocities)))
			  midi-events)
		    (push (cons (nth i end-times)(midi-note-off channel-index (nth i note-list) 64))
			  midi-events))
		  (sort-midi-events midi-events))))) 

	 (get-velocity 
	  (state instrument)
	  (let* ((dynamic-1 (approximate
			     (if (strummer-state-key state)              ;; Only step for 'real' note
				 (next-1 (strummer-state-dynamic state)) ;; events Use current value for grace note
			       (value (strummer-state-dynamic state)))
			     :scale (strummer-state-dynamic-blur state)))
		 (dynamic-2 (limit (funcall (dynamic-map instrument) dynamic-1)
				   (strummer-state-dynamic-min state)
				   (strummer-state-dynamic-max state))))
	    (norm->midi-data dynamic-2)))
	 
	 (get-duration 
	  (state instrument)
	  (funcall (articulation-map instrument)
		   (strummer-state-articulation state)))
	 
	 (render-grace-note 
	  (time state instrument base-velocity)
	  (let ((base-key (strummer-state-grace-key state)))
	    (if base-key
		(let* ((channel-index (channel-index instrument))
		       (grace-key (funcall (keynumber-map instrument) base-key))
		       (grace-velocity (limit (truncate (* base-velocity (strummer-state-grace-amp-scale state))) 1 127))
		       (duration (strummer-state-grace-articulation state))
		       (delay (strummer-state-grace-delay state))
		       (grace-time (max 0 (+ time delay))))
		  (if (or (rest-p grace-key)(zerop grace-velocity)(rest-p duration))
		      (return-from render-grace-note nil))
		  (list
		   (cons grace-time (midi-note-on channel-index grace-key grace-velocity))
		   (cons (+ grace-time duration)(midi-note-off channel-index grace-key 64)))))))

	 (render-note-events 
	  (time strummer state instrument)
	  (let ((velocity (get-velocity state instrument))
		(duration (get-duration state instrument)))
	    (if (or (zerop velocity)(rest-p duration))
		(return-from render-note-events nil))
	    (append
	     (render-grace-note time state instrument velocity)
	     (strum-chord strummer state instrument velocity))))

	 (render-state 
	  (strummer state instrument time-offset)
	  (let* ((midi-events '())
		 (time (+ time-offset (strummer-state-time state)))
		 (channel-index (channel-index instrument))
		 (program-events (render-program-change time state instrument))
		 (note-events (render-note-events time strummer state instrument))
		 )
	    (push? (render-bend time state channel-index) midi-events)
	    (push? (render-control-change time state channel-index) midi-events)
	    (if program-events
	    	(setf midi-events (append midi-events program-events)))
	    (if note-events
	    	(setf midi-events (append midi-events note-events)))
	    (sort-midi-events midi-events))) )

  
  (defmethod pattern-reset ((strummer strummer))
    (dolist (state (strummer-states strummer))
      (pattern-reset state)))
  
  (defmethod render-once ((strummer strummer) &key (offset 0))
    (if (not (muted-p strummer))
  	(let* ((midi-events '())
  	       (instrument (property strummer :instruments)))
	  (pattern-reset strummer)
  	  (dolist  (state (strummer-states strummer))
	    (let ((state-events (render-state strummer state instrument offset)))
	      (setf midi-events (append midi-events state-events))
	      (if *strummer-render-trace*
		  (progn 
		    (format t "~A~%" state)
		    (dump-events state-events)))))
  	  midi-events)))

  (defmethod render-n ((part strummer)(n integer) &key (offset 0.0))
    (let ((period (phrase-duration part))
	  (template (render-once part))
	  (midi-events '()))
      (dotimes (i (if (property part :render-once) 1 n))
	(dolist (event template)
	  (let ((reltime (car event))
		(message (cdr event)))
	    (push (cons (+ offset (* i period) reltime) message) midi-events))))
      (sort-midi-events midi-events))) ) 
