;;;; CYCO parts controllers-render.lisp
;;;;

(in-package :cyco-part)

(labels ((get-time-list
	  (start end interval shift)
	  (let* ((acc '())
		 (diff (float (- end start)))
		 (increment (/ interval diff))
		 (time start))
	    (while (<= time end)
	      (push (+ shift time) acc)
	      (setf time (+ time increment)))
	    (reverse acc)))

	 
	 (generate-single-event
	  (state time-shift channel-index-list)
	  (let ((event-list '())
		(specification (controllers-state-single-event state)))
	    (if (null specification)(return-from generate-single-event))
	    (let ((time (+ time-shift (car specification)))
		  (controller (second specification))
		  (value (third specification)))
	      (dolist (ci channel-index-list)
		(push (cons time (midi-control-change ci controller value))
		      event-list)))
	    event-list))

	 
	 (generate-curve-events
	  (state time-shift channel-index-list)
	  (let ((curve (controllers-state-curve state)))
	    (if (not curve)(return-from generate-curve-events))
	    (let* ((event-list '())
		   (time1 (controllers-state-start-time state))
		   (time2 (controllers-state-end-time state))
		   (interval (controllers-state-time-interval state))
		   (cycles (controllers-state-cycles state))
		   (phase (controllers-state-phase state))
		   (width (controllers-state-width state))
		   (time-list (get-time-list time1 time2 interval time-shift))
		   (steps (length time-list))
		   (value1 (controllers-state-start-value state))
		   (value2 (controllers-state-end-value state))
		   (controller (controllers-state-controller state))
		   (pattern (cond
			     ((eq curve :saw)
			      (isawtooth value1 value2 :steps steps
					 :cycles cycles :phase phase))
			     ((eq curve :tri)
			      (itriangle value1 value2 :steps steps
					 :cycles cycles :phase phase))
			     ((eq curve :pulse)
			      (ipulse value1 value2 :steps steps
				      :cycles cycles :phase phase :width width))
			     (t (iramp value1 value2 :steps steps))))
		   (value-list (next pattern :all)))
	      (loop for time in time-list for value in value-list
		    do (dolist (ci channel-index-list)
			 (push
			  (cons time (midi-control-change ci controller value))
			  event-list)))
	      event-list))) )

  
  (defmethod render-once ((part controllers) &key (offset 0.0))
    (if (muted-p part)(return-from render-once '()))
    (reset part)
    (let* ((midi-events '())
	   (time-shift (+ (property part :shift) offset))
	   (channel-index-list (mapcar #'channel-index (property part :instruments))))
      (dolist (state (controllers-states part))
	(setf midi-events
	      (append midi-events
		      (generate-single-event state time-shift channel-index-list)))
	(setf midi-events
	      (append midi-events
		      (generate-curve-events state time-shift channel-index-list))))
      (if (not (property part :no-thin))
	  (dolist (channel-index channel-index-list)
	    (setf midi-events (thin-controller-events midi-events channel-index))))
      (sort-midi-events midi-events))) )


(defmethod render-n ((part controllers)(n integer) &key (offset 0.0))
  (let* ((period (phrase-duration part))
	 (midi-events '())
	 (template (render-once part :offset offset)))
    (dotimes (i (if (property part :render-once) 1 n))
      (dolist (event template)
	(let ((relative-time (car event))
	      (message (cdr event)))
	  (push (cons (+ (* i period) relative-time)(clone message))
		midi-events))))
    (sort-midi-events midi-events)))

