;;;; CYCO parts controllers-render.lisp
;;;;
;;;; Defines MIDI render methods for CONTROLLERS and BENDER classes.
;;;;

(in-package :cyco-part)

(labels ((get-time-list
	  (start end interval shift)
	  (let* ((acc '())
		 (time start))
	    (while (<= time end)
	      (push (+ shift time) acc)
	      (setf time (+ time interval)))
	    (reverse acc)))

	 
	 (generate-single-controller-event
	  (state time-shift channel-index-list)
	  (let ((event-list '())
		(specification (controllers-state-single-event state)))
	    (if (null specification)(return-from generate-single-controller-event))
	    (let* ((time (+ time-shift (car specification)))
		   (controller (second specification))
		   (value (third specification))
		   (message-function (if (numberp controller)
					 #'(lambda (ci value)(midi-control-change ci controller value))
				       #'midi-channel-pressure)))
	      (dolist (ci channel-index-list)
		(push (cons time (funcall message-function ci value)) event-list)))
	    event-list))


	 (generate-controller-curve-events
	  (state time-shift channel-index-list)
	  (let ((curve (controllers-state-curve state)))
	    (if (not curve)(return-from generate-controller-curve-events))
	    (let* ((event-list '())
		   (time1 (controllers-state-start-time state))
		   (time2 (controllers-state-end-time state))
		   (interval (controllers-state-time-interval state))
		   (cycles (controllers-state-cycles state))
		   (phase (controllers-state-phase state))
		   (width (truncate (* 100 (controllers-state-width state))))
		   (time-list (get-time-list time1 time2 interval time-shift))
		   (steps (length time-list))
		   (value1 (controllers-state-start-value state))
		   (value2 (controllers-state-end-value state))
		   (controller (controllers-state-controller state))
		   (generator (cond
			       ((eq curve :saw)
				(lfo :curve (sawtooth value1 value2 :steps steps
						      :cycles cycles :phase phase)))
			       ((eq curve :tri)
				(lfo :curve (triangle value1 value2 :steps steps
						      :cycles cycles :phase phase)))

			       ((eq curve :pulse)
				(lfo :curve (pulse value1 value2 :steps steps
						   :cycles cycles :phase phase
						   :width width)))
			       (t (let ((delta (/ (float (- value2 value1)) (1- steps))))
				    (ramp value1 value2 :by delta)))))
		   (value-list (mapcar #'round (next generator steps))))
	      (let ((message-function (if (numberp controller)
					  #'(lambda (ci value)(midi-control-change ci controller value))
					#'midi-channel-pressure)))
		(loop for time in time-list for value in value-list
		      do (dolist (ci channel-index-list)
			   (push (cons time (funcall message-function ci value)) event-list))))
	      event-list)))

	 ;; Bend values are signed normalized floats.
	 ;;
	 (generate-single-bend-event
	  (state time-shift channel-index-list)
	  (let ((clause (controllers-state-single-event state)))
	    (if clause
		(let* ((midi-events '())
		       (time (+ time-shift (car clause)))
		       (normal-value (third clause))
		       (bytes (bend->midi-data normal-value))
		       (lsb (aref bytes 0))
		       (msb (aref bytes 1)))
		  (dolist (ci channel-index-list)
		    (push (cons time (midi-pitch-bend ci lsb msb)) midi-events))
		  midi-events))))

	 (generate-bend-curve-events
	  (state time-shift channel-index-list)
	  (flet ((norm->14bit (n)
			      (limit (round (* 8192 (+ n 1))) 0 16383)))
	    (let ((curve (controllers-state-curve state)))
	      (if (not curve)(return-from generate-bend-curve-events))
	      (let* ((event-list '())
		     (time1 (controllers-state-start-time state))
		     (time2 (controllers-state-end-time state))
		     (interval (controllers-state-time-interval state))
		     (cycles (controllers-state-cycles state))
		     (phase (controllers-state-phase state))
		     (width (truncate (* 100 (controllers-state-width state))))
		     (time-list (get-time-list time1 time2 interval time-shift))
		     (steps (length time-list))
		     (norm-value1 (controllers-state-start-value state))
		     (norm-value2 (controllers-state-end-value state))
		     (value1 (norm->14bit norm-value1))
		     (value2 (norm->14bit norm-value2))
		     (generator (cond
				 ((eq curve :saw)
				  (lfo :curve (sawtooth value1 value2 :steps steps
							:cycles cycles :phase phase)))
				 ((eq curve :tri)
				  (lfo :curve (triangle value1 value2 :steps steps
							:cycles cycles :phase phase)))
				 
				 ((eq curve :pulse)
				  (lfo :curve (pulse value1 value2 :steps steps
						     :cycles cycles :phase phase
						     :width width)))
				 (t (let ((delta (/ (float (- value2 value1))(1- steps))))
				      (ramp value1 value2 :by delta)))))
		     (value-list (mapcar #'round (next generator steps)))
		     (lsb-list (mapcar #'(lambda (v)(logand v #x7f)) value-list))
		     (msb-list (mapcar #'(lambda (v)(logand (ash v -7) #x7f)) value-list)))
		(loop for time in time-list for lsb in lsb-list for msb in msb-list
		      do (dolist (ci channel-index-list)
			   (push (cons time (midi-pitch-bend ci lsb msb)) event-list)))
		event-list)))) )
  
  (defmethod render-once ((part controllers) &key (offset 0.0))
    (if (muted-p part)(return-from render-once '()))
    (reset part)
    (let* ((midi-events '())
	   (time-shift (+ (property part :shift) offset))
	   (channel-index-list (mapcar #'channel-index (property part :instruments))))
      (dolist (state (controllers-states part))
	(setf midi-events
	      (append midi-events
		      (generate-single-controller-event state time-shift channel-index-list)
		      (generate-controller-curve-events state time-shift channel-index-list))))
      (if (not (property part :no-thin))
	  (dolist (channel-index channel-index-list)
	    (setf midi-events (thin-controller-events midi-events channel-index))))
      (sort-midi-events midi-events)))

  (defmethod render-once ((part bender) &key (offset 0.0))
    (if (muted-p part)(return-from render-once '()))
    (reset part)
    (let* ((midi-events '())
	   (time-shift (+ (property part :shift) offset))
	   (channel-index-list (mapcar #'channel-index (property part :instruments))))
      (dolist (state (bender-states part))
	(setf midi-events
	      (append midi-events
		      (generate-single-bend-event state time-shift channel-index-list)
		      (generate-bend-curve-events state time-shift channel-index-list))))
      (if (not (property part :no-thin))
	  (dolist (channel-index channel-index-list)
	    (setf midi-events (thin-bend-events midi-events channel-index))))
      (sort-midi-events midi-events))) )
    
