;;;; CYCO3 src/midi/smf-track
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
	    
(defmethod clone ((trk smf-track) &key (new-name "~A") new-parent)
  (dismiss new-parent)
  (let ((other (make-instance 'smf-track
			      :name (sformat new-name (name trk)))))
    (setf (slot-value other 'events)
	  (clone (slot-value trk 'events)))
    other))

(defmethod push-event ((time number)(msg midi-message)(trk smf-track))
  (push (cons (float time) msg)(slot-value trk 'events)))

(flet ((track-name-test
	(trk)
	(if (not (some #'(lambda (q)
			   (midi-meta-track-name-p (cdr q)))
		       (slot-value trk 'events)))
	    (push-event 0 (midi-meta-track-name (name trk)) trk))
	nil)
       
       (track-tempo-test
	(trk)
	(if (not (some #'(lambda (q)
			   (midi-tempo-message-p (cdr q)))
		       (slot-value trk 'events)))
	    (push-event 0 (midi-tempo-message 60.0) trk))
	nil)
       
       (track-timesig-test
	(trk)
	(if (not (some #'(lambda (q)
			   (midi-time-signature-p (cdr q)))
		       (slot-value trk 'events)))
	    (push-event 0 (midi-time-signature 4 'q) trk))
	nil)
       
       (track-keysig-test
	(trk)
	(if (not (some #'(lambda (q)
			   (midi-key-signature-p (cdr q)))
		       (slot-value trk 'events)))
	    (push-event 0 (midi-key-signature 0 nil) trk))
	nil))
  
  (defmethod events ((trk smf-track) &key range (filter #'false))
    (track-name-test trk)	        ; gurentee events list contains at 
    (track-tempo-test trk)		; least one each track-name, tempo,
    (track-timesig-test trk)	        ; timesig and keysig event.
    (track-keysig-test trk)
    (let ((acc (sort (copy-list (slot-value trk 'events))
		     #'(lambda (a b)
			 (let ((ta (car a))
			       (tb (car b)))
			   (if (= ta tb)
			       (< (priority (cdr a))
				  (priority (cdr b)))
			     (< ta tb))))))
	  (t1 (or (car range) 0))
	  (t2 (or (car (cdr range)) 1e9))
	  (bcc '()))
      (dolist (p acc)
	(let ((ta (car p))
	      (ev (cdr p)))
	  (if (and (>= ta t1)(<= ta t2)(not (funcall filter ev)))
	      (push p bcc))))
      (reverse bcc))))

(defmethod dump-events ((trk smf-track) &key range (filter #'false)(render nil))
  (let ((lst (events trk :range range :filter filter)))
    (dolist (p lst)
      (let* ((time (car p))
	     (ev (cdr p))
	     (s (sformat  "[~,4f] " time)))
	(format t "~12A ~36A  " s (->string ev))
	(if render
	    (progn 
	      (format t "MIDI[ ")
	      (dolist (b (render-midi-message ev))
		(format t "~2X " b))
	      (format t "]")))
	(format t "~%")))
    (length lst)))

(defmethod dump-events ((lst list) &key range filter render)
  (setf filter (or filter #'false))
  (if (every #'(lambda (q)
		 (and (consp q)
		      (numberp (car q))
		      (midi-message-p (cdr q))))
	     lst)
      (let ((acc (sort lst #'(lambda (a b) ;; ISSUE: factor out, use sort-midi-events 
			       (let ((ta (car a))
				     (tb (car b)))
				 (if (= ta tb)
				     (< (priority (cdr a))
					(priority (cdr b)))
				   (< ta tb))))))
	    (t1 (or (car range) 0))
	    (t2 (or (cdr range) 1e9)) )
	(dolist (p acc)
	  (let* ((time (car p))
		 (ev (cdr p))
		 (s (sformat "[~,4f] " time)))
	    (if (and (>= time t1)(<= time t2)(not (funcall filter ev)))
		(progn
		  (format t "~A ~46A  " s (->string ev))
		  (if render
		      (progn 
			(format t "MIDI[ ")
			(dolist (b (render-midi-message ev))
			  (format t "~2X " b))
			(format t "]")))
		  (format t "~%")))))
	(length lst))
    (cyco-error "Invalid MIDI track")))

(flet ((render-events
	(trk pad)
	(let* ((events (events trk))
	       (end-time (+ (car (car (reverse events))) 
			    (float pad)))
	       (beat-unit 'q)
	       (tickdur (tick-duration 60 
				       :unit beat-unit))
	       (previous-time 0.0)
	       (acc '()))
	  (dolist (p (append events
			     (list (cons end-time 
					 (midi-end-of-track)))))
	    (let* ((event-time (car p))
		   (event (cdr p))
		   (delta (prog1
			      (- event-time previous-time)
			    (setf previous-time event-time)))
		   (ticks (truncate (/ delta tickdur))))
	      (cond ((midi-time-signature-p event)
		     (setf beat-unit (timesig-unit event)))
		    ((midi-tempo-message-p event)
		     (setf tickdur (tick-duration 
				    (slot-value event 'tempo) 
				    :unit beat-unit)))
		    (t nil))
	      (push (int->midi-vlv ticks) acc)
	      (push (render-midi-message event) acc)))
	  (flatten (reverse acc)))))
  
  ;; Renders track chunk, including ID, data-count and data.
  (defmethod render-smf-track ((trk smf-track)(pad number))
    (let* ((data (render-events trk pad))
	   (bcount (length data)))
      (append (list #x4D #x54 #x72 #x6B
		    (logand (ash bcount -24) #xFF)
		    (logand (ash bcount -16) #xFF)
		    (logand (ash bcount -8) #xFF)
		    (logand bcount #xFF))
	      data))))

