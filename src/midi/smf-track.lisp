;;;; CYCO
;;;;

(defclass smf-track nil
  ((track-name
    :type string
    :reader name
    :initform "Track"
    :initarg :name)
   (events
    :type list
    :initform '()
    :initarg :events)))
	    
(defmethod clone ((track smf-track) &key (new-name "~A") new-parent)
  (dismiss new-parent)
  (let ((new-track (make-instance 'smf-track
				  :name (sformat new-name (name track)))))
    (setf (slot-value new-track 'events)
	  (clone (slot-value track 'events)))
    new-track))

(defmethod push-event ((time number)(message midi-message)(track smf-track))
  (push (cons (float time) message)
	(slot-value track 'events)))
  
(defmethod events ((track smf-track) &key range (filter #'false))
  (let ((source-events (sort-midi-events (slot-value track 'events)))
	(t1 (or (car range) 0))
	(t2 (or (car (cdr range)) 1e9))
	(result '()))
    (dolist (p source-events)
      (let ((ta (car p))
	    (ev (cdr p)))
	(if (and (>= ta t1)(<= ta t2)(not (funcall filter ev)))
	    (push p result))))
    (reverse result)))


(defmethod dump-events ((track smf-track) &key range (filter #'false)(render nil))
  (let ((event-list (events track :range range :filter filter)))
    (dolist (event event-list)
      (let* ((time (car event))
	     (message (cdr event))
	     (time-string (sformat  "[~,4f] " time)))
	(format t "~12A ~36A  " time-string (->string message))
	(if render
	    (progn 
	      (format t "MIDI[ ")
	      (dolist (event (render-midi-message message))
		(format t "~2X " event))
	      (format t "]")))
	(format t "~%")))
    (length event-list)))


(defmethod dump-events ((event-list list) &key range (filter #'false) render)
  (if (every #'(lambda (event)
		 (and (consp event)
		      (numberp (car event))
		      (midi-message-p (cdr event))))
	     event-list)
      (let ((t1 (or (car range) 0))
	    (t2 (or (cdr range) 1e9)))
	(dolist (event (sort-midi-events event-list))
	  (let* ((time (car event))
		 (message (cdr event))
		 (time-string (sformat "[~,4f] " time)))
	    (if (and (>= time t1)(<= time t2)(not (funcall filter message)))
		(progn
		  (format t "~A ~46A  " time-string (->string message))
		  (if render
		      (progn 
			(format t "MIDI[ ")
			(dolist (b (render-midi-message message))
			  (format t "~2X " b))
			(format t "]")))
		  (format t "~%")))))
	(length event-list))
    (cyco-error "Invalid MIDI track")))

(flet ((render-events (track pad)
		      (let* ((source-events (events track))
			     (end-time (+ (caar (reverse source-events)) (float pad)))
			     (beat-unit 'q)
			     (tickdur (tick-duration 60 :unit beat-unit))
			     (previous-time 0.0)
			     (events '()))
			(setf source-events (append source-events (list (cons end-time (midi-end-of-track)))))
			(dolist (event source-events)
			  (let* ((event-time (car event))
				 (message (cdr event))
				 (delta (prog1
					    (- event-time previous-time)
					  (setf previous-time event-time)))
				 (ticks (truncate (/ delta tickdur))))
			    (cond ((midi-time-signature-p message)
				   (setf beat-unit (timesig-unit message)))
				  ((midi-tempo-message-p message)
				   (setf tickdur (tick-duration 
						  (slot-value message 'tempo) 
						  :unit beat-unit)))
				  (t nil))
			    (push (int->midi-vlv ticks) events)
			    (push (render-midi-message message) events)))
			(flatten (reverse events)))))
  
  ;; Renders track chunk, including ID, data-count and data.
  (defmethod render-smf-track ((track smf-track)(pad number))
    (let* ((data (render-events track pad))
	   (bcount (length data)))
      (append (list #x4D #x54 #x72 #x6B
		    (logand (ash bcount -24) #xFF)
		    (logand (ash bcount -16) #xFF)
		    (logand (ash bcount -8) #xFF)
		    (logand bcount #xFF))
	      data))))



