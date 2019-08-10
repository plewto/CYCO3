;;;; cyco-strummer midi-render.lisp


(param *trace-strummer-render* t)

(defmacro push? (item list) `(if ,item (push ,item ,list)))


(defun render-bend (time state channel-index)
  (let ((normalized-bend (state-bend state)))
    (if normalized-bend
	(let ((data (bend->midi-data normalized-bend)))
	  (cons time (midi-pitch-bend channel-index (aref data 0)(aref data 1))))
      nil)))
    	 
(defun render-control-change (time state channel-index)
  (let ((controller-number (state-controller-number state))
	(normalized-value (state-controller-value state)))
    (if (and controller-number normalized-value)
	(let ((midi-data (norm->midi-data normalized-value)))
	  (cons time (midi-control-change channel-index controller-number midi-data)))
      nil)))

(defun render-program-change (time state instrument)
  (let ((program (state-program-number state))
	(bank (state-program-bank state)))
    (if program
	(let ((pmap (program-map instrument)))
	  (funcall pmap time :bank bank :program program))
      nil)))

(labels ((resolve-chord (strummer state instrument)
			(dismiss strummer state instrument)
			'(dummy not list))

	 (purge-rest (note-list)
		     (remove-if #'rest-p note-list))

	 (prepare-note-list (strummer state root-key)
			    (let* ((chord-model (property strummer :chord-model))
				   (degree (state-chord-inversion state))
				   (octave (state-chord-octave state))
				   (direction (next-1 (state-strum-direction state)))
				   (chord-type (state-chord-type state))
				   (note-list (if (listp chord-type)
						  chord-type
						(chord-template chord-model chord-type root-key))))
			      (setf note-list (if (absolute-chords-p chord-model)
						  note-list
						(mapcar #'(lambda (q)(+ q root-key)) note-list)))
			      (setf note-list (cond ((eq direction :up)
						     (reverse note-list))
						    ((eq direction :dice)
						     (if (< (random 1000) 500)
							 (reverse note-list)
						       note-list))
						    ((eq direction :random)
						     (permute note-list))
						    (t note-list)))
			      (chord-inversion note-list degree :add-octave octave)))
	 
	 (prepare-note-start-times (state note-count)
				   (let ((time (state-time state))
					 (delay (state-strum-delay state))
					 (acceleration (state-strum-acceleration state))
					 (acc '()))
				     (dotimes (i note-count)
				       (push time acc)
				       (setf time (+ time delay))
				       (setf delay (* delay acceleration)))
				     (reverse acc)))

	 (prepare-note-end-times (state start-times instrument)
				 (let ((acc '())
				       (duration (funcall (articulation-map instrument)
							  (state-articulation state))))
				   (dolist (start start-times)
				     (push (+ start duration) acc))
				   (if (state-strum-end-together state)
				       (copies (length start-times)(apply #'max acc))
				     (reverse acc))))

	 (prepare-note-velocities (state note-count root-velocity)
				  (let ((scale (state-strum-amp-scale state))
					(velocity (float root-velocity))
					(acc '()))
				    (dotimes (i note-count)
				      (push (truncate (limit velocity 1 127)) acc)
				      (setf velocity (* velocity scale)))
				  (reverse acc))) )
  
  (defun strum-chord (strummer state instrument root-velocity)
    (let ((root-key (funcall (keynumber-map instrument)(state-key state))))
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
	    (sort-midi-events midi-events)))))) 

(labels ((get-velocity (state instrument)
		       (let* ((dynamic-1 (approximate
					  (if (state-key state)              ;; Only step for 'real' note
					      (next-1 (state-dynamic state)) ;; events
					    (value (state-dynamic state))))) ;; Use current value for grace note
			      (dynamic-2 (limit (funcall (dynamic-map instrument) dynamic-1)
						(state-dynamic-min state)
						(state-dynamic-max state))))
			 (norm->midi-data dynamic-2)))
	 
	 (get-duration (state instrument)
		       (funcall (articulation-map instrument)
				(state-articulation state)))

	 (render-grace-note (time state instrument base-velocity)
			    (let ((base-key (state-grace-key state)))
			      (if base-key
				  (let* ((channel-index (channel-index instrument))
					 (grace-key (funcall (keynumber-map instrument) base-key))
					 (grace-velocity (limit (truncate (* base-velocity (state-grace-amp-scale state))) 1 127))
					 (duration (state-grace-articulation state))
					 (delay (state-grace-delay state))
					 (grace-time (max 0 (+ time delay))))
				    (if (or (rest-p grace-key)(zerop grace-velocity)(rest-p duration))
					(return-from render-grace-note nil))
				    (list
				     (cons grace-time (midi-note-on channel-index grace-key grace-velocity))
				     (cons (+ grace-time duration)(midi-note-off channel-index grace-key 64))))))) )

  (defun render-note-events (time strummer state instrument)
    (let ((velocity (get-velocity state instrument))
	  (duration (get-duration state instrument)))
      (if (or (zerop velocity)(rest-p duration))
	  (return-from render-note-events nil))
	  (append (render-grace-note time state instrument velocity)
	  	  (strum-chord strummer state instrument velocity)))) ) 

(defun render-state (strummer state instrument time-offset)
  (let ((midi-events '())
	(time (+ time-offset (state-time state)))
	(channel-index instrument))
    (push? (render-bend time state channel-index) midi-events)
    (push? (render-control-change time state channel-index) midi-events)
    (push? (render-program-change time state instrument) midi-events)
    (let ((note-events (render-note-events time strummer state instrument)))
      (if note-events
	  (setf midi-events (append midi-events note-events))))
    (sort-midi-events midi-events)))

(defmethod render-once ((strummer strummer) &key (offset 0))
  (if (not (muted-p strummer))
      (let* ((midi-events '())
	     (instrument (property strummer :instruments)))
	(dolist  (state (strummer-states strummer))
	  (setf midi-events (append midi-events (render-state strummer state instrument offset))))
	midi-events)))
