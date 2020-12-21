;;;; CYCO midi smf-track.lisp
;;;;
;;;; Defines MIDI file track.
;;;;

(in-package :cyco)

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
	    
(defmethod clone ((mother smf-track) &key (new-name "~A") new-parent)
  (declare (ignore new-parent))
  (let ((daughter (make-instance
		   'smf-track
		   :name (sformat new-name (name mother)))))
    (setf (slot-value daughter 'events)
	  (clone (slot-value mother 'events)))
    daughter))

(defmethod push-event ((time number)(message midi-message)(track smf-track))
  (push (cons (float time) message)
	(slot-value track 'events)))
  
(defmethod events ((track smf-track) &key range (filter #'false))
  (let ((events (sort-midi-events (slot-value track 'events)))
	(time-range-start (or (car range) 0))
	(time-range-end (or (car (cdr range)) 1e9))
	(result '()))
    (dolist (event events)
      (let ((event-time (car event))
	    (message (cdr event)))
	(if (and (>= event-time time-range-start)
		 (<= event-time time-range-end)
		 (not (funcall filter message)))
	    (push event result))))
    (reverse result)))

(defmethod dump-events ((track smf-track) &key range (filter #'false)(render nil))
  (let ((events (events track :range range :filter filter)))
    (dolist (event events)
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
    (length events)))

(defmethod dump-events ((events list) &key range (filter #'false) render)
  (if (every #'(lambda (event)
		 (and (consp event)
		      (numberp (car event))
		      (midi-message-p (cdr event))))
	     events)
      (let ((time-range-start (or (car range) 0))
	    (time-range-end (or (cdr range) 1e9)))
	(dolist (event (sort-midi-events events))
	  (let* ((event-time (car event))
		 (message (cdr event))
		 (time-string (sformat "[~,4f] " event-time)))
	    (if (and (>= event-time time-range-start)
		     (<= event-time time-range-end)
		     (not (funcall filter message)))
		(progn
		  (format t "~A ~46A  " time-string (->string message))
		  (if render
		      (progn 
			(format t "MIDI[ ")
			(dolist (byte (render-midi-message message))
			  (format t "~2X " byte))
			(format t "]")))
		  (format t "~%")))))
	(length events))
    (cyco-error "Invalid MIDI event list")))

(flet ((render-events (track pad)
		      (let* ((source-events (events track))
			     (end-time (+ (caar (reverse source-events))(float pad)))
			     (beat-unit 'q)
			     (tick-duration (tick-duration 60 :unit beat-unit))
			     (previous-time 0.0)
			     (events '()))
			(setf source-events (append source-events (list (cons end-time (midi-end-of-track)))))
			(dolist (event source-events)
			  (let* ((event-time (car event))
				 (message (cdr event))
				 (delta (prog1
					    (- event-time previous-time)
					  (setf previous-time event-time)))
				 (ticks (truncate (/ delta tick-duration))))
			    (cond ((midi-time-signature-p message)
				   (setf beat-unit (timesig-unit message)))
				  ((midi-tempo-message-p message)
				   (setf tick-duration (tick-duration 
						  (slot-value message 'tempo) 
						  :unit beat-unit)))
				  (t nil))
			    (push (int->midi-vlv ticks) events)
			    (push (render-midi-message message) events)))
			(flatten (reverse events)))))
  
  ;; Renders track chunk, including ID, data-count and data.
  (defmethod render-smf-track ((track smf-track)(pad number))
    (let* ((data (render-events track pad))
	   (byte-count (length data)))
      (append (list #x4D #x54 #x72 #x6B
		    (logand (ash byte-count -24) #xFF)
		    (logand (ash byte-count -16) #xFF)
		    (logand (ash byte-count -8) #xFF)
		    (logand byte-count #xFF))
	      data))))



