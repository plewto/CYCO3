(in-package :cyco-part)

;; event-type may be one of following
;;    :bend      --> pitch bend
;;    :presasure --> channel poressure
;;    integer    --> control change, where event-type is cxontroller-number.
;;

(labels ((convert-to-midi-data
	  ;; For :bend returns vector #(low high)
	  ;; otherwise returns integer 0 < n < 128.
	  (normalized-value event-type curve-function)
	  (let ((norm2 (funcall curve-function normalized-value)))
	    (if (eq event-type :bend)
		(bend->midi-data norm2)
	      (norm->midi-data norm2))))

	 (create-event
	  (time instrument event-type value)
	  (let ((ci (channel-index instrument)))
	    (cons time (cond ((eq event-type :bend)
			      (let ((lsb (aref value 0))
				    (msb (aref value 1)))
				(midi-pitch-bend ci lsb msb)))
			     ((eq event-type :pressure)
			      (midi-channel-pressure ci value))
			     (t (midi-control-change ci event-type value)))))) )

  (defmethod render-once ((part controllers) &key (offset 0.0))
    (if (not (muted-p part))
	(let* ((midi-events '())
	       (cuefn (property part :cue-function))
	       (instrument-list (->list (property part :instruments))))
	  (dolist (state (controllers-states part))
	    (let* ((event-type (controllers-state-event-type state))
		   (steps (controllers-state-steps state))
		   (t1 (+ offset (funcall cuefn part (controllers-state-time-start state))))
		   (t2 (+ offset (funcall cuefn part (controllers-state-time-end state))))
		   (delta-t (float (abs (- t2 t1))))
		   (time-increment (/ delta-t (1- steps)))
		   (v1 (limit (controllers-state-value-start state) -1 1))
		   (v2 (limit (controllers-state-value-end state) -1 1))
		   (delta-v (float (- v2 v1)))
		   (value-increment (/ delta-v (1- steps)))
		   (norm-value v1)
		   (time (min t1 t2))
		   (curve-function (property part :curve-function)) )
	      (dotimes (i steps)
		(let* ((midi-value (convert-to-midi-data norm-value event-type curve-function)))
		  (dolist (instrument instrument-list)
		    (push (create-event time instrument event-type midi-value) midi-events))
		  (setf time (+ time time-increment))
		  (setf norm-value (+ norm-value value-increment))))))
	  (sort-midi-events midi-events)))) )

(defmethod render-n ((part controllers)(n integer) &key (offset 0.0))
  (let ((period (phrase-duration part))
	(template (render-once part))
	(midi-events '()))
    (dotimes (i (if (property part :render-once) 1 n))
      (dolist (event template)
	(let ((reltime (car event))
	      (message (cdr event)))
	  (push (cons (+ offset (* i period) reltime) message) midi-events))))
    (sort-midi-events midi-events)))
	
