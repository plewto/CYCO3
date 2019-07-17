;;;; CYCO
;;;; Strummer rendering functions.
;;;;

(global *strummer-render-trace* nil)

(labels ((process-bend (midi-event-list time state channel-index)
		       (let ((bend (strummer-state-bend state)))
			 (if bend
			     (let* ((data (bend->midi-data bend))
				    (lsb (aref data 0))
				    (msb (aref data 1)))
			       (cons (cons time (midi-pitch-bend channel-index lsb msb)) midi-event-list))
			   midi-event-list)))
	 
	 (process-controller (midi-event-list time state channel-index)
			     (let ((controller-number (strummer-state-controller-number state))
				   (value (strummer-state-controller-value state)))
			       (if (and controller-number value)
				   (let ((data (norm->midi-data value)))
				     (cons (cons time (midi-control-change channel-index controller-number data)) 
					   midi-event-list))
				 midi-event-list)))
	 
	 (process-program (midi-event-list time state instrument)
			  (let ((program (strummer-state-program-number state)))
			    (if program 
				(let ((bank (strummer-state-program-bank state))
				      (program-map (property instrument :program-map)))
				  (setf midi-event-list (append midi-event-list (funcall program-map time 
											 :bank bank :program program))))
			      midi-event-list)))
	 
	 (simple-note-events (time channel-index keynumber velocity duration)
			     (list (cons time (midi-note-on channel-index keynumber velocity))
				   (cons (+ time duration)
					 (midi-note-off channel-index keynumber 0))))

	 (process-grace-note (midi-event-list time state instrument)
			     (let ((grace-key (strummer-state-grace-key state)))
			       (if grace-key
				   (let ((key-number (funcall (keynumber-map instrument) grace-key))
					 (duration (funcall (articulation-map instrument)
						     (strummer-state-grace-articulation state)))
					 (velocity (norm->midi-data
						    (funcall (dynamic-map instrument)
							     (* (value (strummer-state-dynamic state))
								(strummer-state-grace-amp-scale state)))))
					 (delay (strummer-state-grace-delay state)))
				     (if (not (rest-p key-number))
					 (let ((channel-index (channel-index instrument)))
					   (append midi-event-list (list (cons (+ delay time) (midi-note-on channel-index key-number velocity))
									 (cons (+ delay time duration)(midi-note-off channel-index key-number 0)))))
				       midi-event-list))
				 midi-event-list)))

	 (expand-chord-template (state chord-model base-key instrument)
				(let ((key-number (funcall (keynumber-map instrument) base-key)))
				  (if (rest-p key-number)(return-from expand-chord-template '(r)))
				  (let* ((chrod-type (strummer-state-chord-type state))
					 (template (if (listp chrod-type)
						       chrod-type
						     (chord-template chord-model chrod-type key-number))))
				    (if (or (listp chrod-type)(not (absolute-chords-p chord-model)))
					(let ((acc '()))
					  (dolist (offset template)
					    (if (not (rest-p offset))
						(push (+ offset key-number) acc)))
					  (setf template (reverse acc))))
				    (chord-inversion
				     template
				     (strummer-state-chord-inversion state)
				     :add-octave (strummer-state-chord-octave state)))))

	 (expand-strum-times (start-time chord-template state)
			     (let ((time start-time)
				   (delay (strummer-state-strum-delay state))
				   (scale (strummer-state-strum-acceleration state))
				   (acc '()))
			       (dotimes (i (length chord-template))
				 (push time acc)
				 (setf time (+ time delay))
				 (setf delay (* delay scale)))
			       (->vector (reverse acc))))
	 
	 ;; start-times as vector
	 ;;
	 (expand-staggered-end-times (start-times duration)
				     (let ((acc '()))
				       (dotimes (i (length start-times))
					 (push (+ (aref start-times i) duration) acc))
				       (->vector (reverse acc))))

	 ;; start-times as vector
	 (expand-homogeneous-end-times (start-times duration)
				       (let ((end-time (+ (apply #'max (->list start-times)) duration)))
					 (->vector (copies (length start-times) end-time))))

	 ;; start times as vector
	 ;; return vector
	 (expand-end-times (start-times duration state)
			   (if (strummer-state-strum-end-together state)
			       (expand-homogeneous-end-times start-times duration)
			     (expand-staggered-end-times start-times duration)))
	 
	 (expand-dynamics (state count base-amp)
			  (let ((acc '())
				(amp-scale (strummer-state-strum-amp-scale state))
				(amp base-amp))
			    (dotimes (i count)
			      (push amp acc)
			      (setf amp (limit (* amp amp-scale) 0 1)))
			    (->vector (reverse acc))))

	 ;; chord-template as list
	 ;; returns vector
	 (select-strum-direction (state chord-template)
				 (let ((direction (next-1 (strummer-state-strum-direction state))))
				   (->vector (cond ((eq direction 'down)
						    chord-template)
						   ((eq direction 'up)
						    (reverse chord-template))
						   ((eq direction 'coin)
						    (if (> (random 1.0) 0.5)
							chord-template
						      (reverse chord-template)))
						   ((eq direction 'random)
						    (permute chord-template))
						   (t
						    (cyco-error
						     (sformat "Invalid STRUMMER STRUM-DIRECTION ~A" direction)))))))

	 
	 (strum-chord (state start-time channel-index chord-template duration base-amp)
		      (let* ((count (length chord-template))
			     (note-on-times (expand-strum-times start-time chord-template state))
			     (note-off-times (expand-end-times note-on-times duration state))
			     (dynamics (expand-dynamics state count base-amp))
			     (dynamic-min (strummer-state-dynamic-min state))
			     (dynamic-max (strummer-state-dynamic-max state))
			     (note-list (select-strum-direction state chord-template))
			     (acc '()))
			(dotimes (i count)
			  (let ((key-number (aref note-list i)))
			    (if (not (rest-p key-number))
				(let ((on-time (aref note-on-times i))
				      (off-time (aref note-off-times i))
				      (velocity (norm->midi-data
						 (limit (aref dynamics i)
							dynamic-min
							dynamic-max))))
				  (push (cons on-time (midi-note-on channel-index key-number velocity)) acc)
				  (push (cons off-time (midi-note-off channel-index key-number 0)) acc)))))
			(reverse acc)))
	 
	 (process-key-events (midi-event-list time state chord-model instrument)
			     (let* ((channel-index (channel-index instrument))
				    (base-key (let ((bk (strummer-state-key state)))
						(if (or (null bk)(rest-p bk))
						    (return-from process-key-events midi-event-list))
						bk))
				    (chord-template (expand-chord-template state chord-model base-key instrument))
				    (base-amp (funcall (dynamic-map instrument)
						       (approximate
							(dynamic (next-1 (strummer-state-dynamic state)))
							:scale (strummer-state-dynamic-blur state)
							:max (strummer-state-dynamic-max state)
							:min (strummer-state-dynamic-min state))))
				    (base-dur (funcall (articulation-map instrument)
						       (or (strummer-state-articulation state) 1.0))))
			       (setf midi-event-list (append midi-event-list (strum-chord state time channel-index chord-template base-dur base-amp))))
			     midi-event-list)
	 
	 (trace-events (state)
		       (if *strummer-render-trace*
			   (progn 
			     (format t "---------------------------------------------~%")
			     (format t "~A~%" state)))) )

  (defmethod render-once ((part strummer) &key (offset 0))
    (if (not (muted-p part))
	(let* ((midi-events '())
	       (shuffle-function (property part :shuffle-function))
	       (chord-model (property part :chord-model))
	       (instrument (property part :instruments))
	       (channel-index (channel-index instrument)))
	  (dolist (state (strummer-events part))
	    (trace-events state)
	    (let* ((state-time (strummer-state-time state))
		   (time (+ offset (if state-time
				       (+ state-time
					  (funcall shuffle-function (strummer-state-time-specification state)))
				     0))))
	      (setf midi-events (process-bend midi-events time state channel-index))
	      (setf midi-events (process-controller midi-events time state channel-index))
	      (setf midi-events (process-program midi-events time state instrument))
	      (setf midi-events (process-grace-note midi-events time state instrument))
	      (setf midi-events (process-key-events midi-events time state chord-model instrument))))
	  midi-events))))
			  
(defmethod render-n ((strummer strummer)(n integer) &key (offset 0.0))
  (let ((period (phrase-duration strummer))
	(template (render-once strummer))
	(midi-events '()))
    (dotimes (i (if (property strummer :render-once) 1 n))
      (dolist (event template)
	(let ((reltime (car event))
	      (message (cdr event)))
	  (push (cons (+ offset (* i period) reltime) message) midi-events))))
    (sort-midi-events midi-events)))



