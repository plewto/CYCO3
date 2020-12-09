;;;; CYCO parts control-ball-midi-render.lisp
;;;;
;;;; Defines rendering methods for control-ball.
;;;;

(in-package :cyco-part)

(labels ((mapping-function (control-ball)
	    (let ((controller (property control-ball :controller)))
	      (cond ((eq controller :bend) #'bend->midi-data)
		    (t #'norm->midi-data))))
				   
	 (get-channel-index-list (control-ball)
	    (mapcar #'channel-index (property control-ball :instruments)))

	 (create-message (controller channel-index data)
	    (cond ((integerp controller)
		   (midi-control-change channel-index controller data))
		  ((eq controller :pressure)
		   (midi-channel-pressure channel-index data))
		  (t (let ((lsb (aref data 0))
			   (msb (aref data 1)))
		       (midi-pitch-bend channel-index lsb msb)))))
	 
	 (render-single-events (control-ball start-time end-time)
	    (let* ((data-mapping-function (mapping-function control-ball))
		   (controller (property control-ball :controller))
		   (midi-events '())
		   (initial-norm-value (property control-ball :initial-value))
		   (final-norm-value (property control-ball :final-value))
		   (channel-index-list (and (or initial-norm-value final-norm-value)
					    (get-channel-index-list control-ball))) )
	      (if initial-norm-value
		  (let* ((value (funcall data-mapping-function initial-norm-value))
			 (shift (let* ((n (or (property control-ball :initial-value-time-shift) 0.01))
				       (scale (if (numberp n)
						  1.0
						(float (beat-duration control-ball)))))
				  (* scale (metric-expression n))))
			 (time (max 0 (- start-time shift))))
		    (dolist (ci channel-index-list)
		      (push (cons time (create-message controller ci value)) midi-events))))
	      (if final-norm-value
		  (let* ((value (funcall data-mapping-function final-norm-value))
			 (shift (let* ((n (or (property control-ball :final-value-time-shift) 0.01))
				       (scale (if (numberp n)
						  1.0
						(float (beat-duration control-ball)))))
				  (* scale (metric-expression n))))
			 (time (+ end-time shift)))
		    (dolist (ci channel-index-list)
		      (push (cons time (create-message controller ci value)) midi-events))))
	      midi-events)) )

  (defmethod render-once ((control-ball control-ball) &key (offset 0.0))
    (if (muted-p control-ball)(return-from render-once '()))
    (if (property control-ball :reset-on-repeat)(reset control-ball))
    (let* ((midi-events '())
	   (cuefn (property control-ball :cue-function))
	   (shuffle (property control-ball :shuffle-function))
	   (start-time (+ (funcall cuefn control-ball (property control-ball :start-cue))
			  (funcall shuffle (property control-ball :start-cue))))
	   (current-time start-time)
	   (end-time (+ (funcall cuefn control-ball (property control-ball :end-cue))
			(funcall shuffle (property control-ball :end-cue))))
	   (time-interval (scale-time-parameter (property control-ball :time-interval) control-ball))
	   (time-shift (+ offset (property control-ball :shift)))
	   (channel-index-list (get-channel-index-list control-ball))
	   (pattern (property control-ball :value-pattern))
	   (controller (property control-ball :controller))
	   (trim-count (property control-ball :trim))
	   (data-map-function (mapping-function control-ball)))
      (if pattern
	  (while (<= current-time end-time)
	    (let* ((norm-value (next pattern))
		   (data (funcall data-map-function norm-value))
		   (time (+ current-time time-shift)))
	      (dolist (ci channel-index-list)
		(push (cons time (create-message controller ci data)) midi-events))
	      (setf current-time (+ current-time time-interval)))))
      (setf midi-events (append (elide midi-events :start (or trim-count 0))
				(render-single-events control-ball start-time end-time)))
      (sort-midi-events midi-events))) )


(defmethod render-n ((control-ball control-ball)(n integer) &key (offset 0.0))
  (reset control-ball)
  (let ((period (phrase-duration control-ball))
	(midi-events '()))
    (dotimes (i (if (property control-ball :render-once) 1 n))
      (dolist (event (render-once control-ball))
	(let ((relative-time (car event))
	      (message (cdr event)))
	  (push (cons (+ offset (* i period) relative-time)(clone message))
		midi-events))))
    (sort-midi-events midi-events)))

	     
			    
