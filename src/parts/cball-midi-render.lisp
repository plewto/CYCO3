(in-package :cyco-part)

(labels ((mapping-function (cball)
	    (let ((controller (property cball :controller)))
	      (cond ((eq controller :bend) #'bend->midi-data)
		    (t #'norm->midi-data))))
				   
	 (get-channel-index-list (cball)
	    (mapcar #'channel-index (property cball :instruments)))

	 (create-message (controller channel-index data)
	    (cond ((integerp controller)
		   (midi-control-change channel-index controller data))
		  ((eq controller :pressure)
		   (midi-channel-pressure channel-index data))
		  (t (let ((lsb (aref data 0))
			   (msb (aref data 1)))
		       (midi-pitch-bend channel-index lsb msb)))))
	 
	 (render-single-events (cball start-time end-time)
	    (let* ((data-mapping-function (mapping-function cball))
		   (controller (property cball :controller))
		   (midi-events '())
		   (initial-norm-value (property cball :initial-value))
		   (final-norm-value (property cball :final-value))
		   (channel-index-list (and (or initial-norm-value final-norm-value)
					    (get-channel-index-list cball))) )
	      (if initial-norm-value
		  (let* ((value (funcall data-mapping-function initial-norm-value))
			 (shift (or (property cball :initial-value-time-shift) 0.01))
			 (time (max 0 (- start-time shift))))
		    (dolist (ci channel-index-list)
		      (push (cons time (create-message controller ci value)) midi-events))))
	      (if final-norm-value
		  (let* ((value (funcall data-mapping-function final-norm-value))
			 (shift (or (property cball :final-value-time-shift) 0.01))
			 (time (+ end-time shift)))
		    (dolist (ci channel-index-list)
		      (push (cons time (create-message controller ci value)) midi-events))))
	      midi-events)) )

  (defmethod render-once ((cball cball) &key (offset 0.0))
    (if (muted-p cball)(return-from render-once nil))
    (if (property cball :reset-on-repeat)(reset cball))
    (let* ((midi-events '())
	   
	   (cuefn (property cball :cue-function))
	   (start-time (funcall cuefn cball (property cball :start-cue)))
	   (current-time start-time)
	   (end-time (funcall cuefn cball (property cball :end-cue)))
	   (time-increment (let* ((ti (property cball :time-increment))
				  (scale (if (numberp ti)
					     1.0
					   (beat-duration cball))))
			     (* scale (metric-expression ti))))
	   (shift (property cball :shift))
	   (channel-index-list (get-channel-index-list cball))
	   (pattern (property cball :value-pattern))
	   (controller (property cball :controller))
	   (data-map-function (mapping-function cball)))
      (if pattern
	  (while (<= current-time end-time)
	    (let* ((norm-value (next pattern))
		   (data (funcall data-map-function norm-value))
		   (time (+ current-time shift offset)))
	      (dolist (ci channel-index-list)
		(push (cons time (create-message controller ci data)) midi-events))
	      (setf current-time (+ current-time time-increment)))))
      (setf midi-events
	    (append midi-events
		    (render-single-events cball start-time end-time)))
      (sort-midi-events midi-events))) )


(defmethod render-n ((cball cball)(n integer) &key (offset 0.0))
  (reset cball)
  (let ((period (phrase-duration cball))
	(midi-events '()))
    (dotimes (i (if (property cball :render-once) 1 n))
      (dolist (event (render-once cball))
	(let ((relative-time (car event))
	      (message (cdr event)))
	  (push (cons (+ offset (* i period) relative-time)(clone message))
		midi-events))))
    (sort-midi-events midi-events)))

	     
			    
