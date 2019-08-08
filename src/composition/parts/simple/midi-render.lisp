;;;; cyco-simple midi-render
;;;; MIDI rendering functions for simple-part

(defun render-bend-events (time state instrument-list)
  (let ((acc '())
	(bend (simple-state-bend state)))
    (if bend
	(let* ((data (bend->midi-data bend))
	       (lsb (aref data 0))
	       (msb (aref data 1)))
	  (dolist (instrument instrument-list)
	    (push (cons time (midi-pitch-bend (channel-index instrument) lsb msb)) acc))))
    acc))

(defun render-controller-events (time state instrument-list)
  (let ((acc '())
	(controller-number (simple-state-controller-number state))
	(normalized-value (simple-state-controller-value state)))
    (if controller-number
	(let ((midi-value (norm->midi-data normalized-value)))
	  (dolist (instrument instrument-list)
	    (push (cons time (midi-control-change (channel-index instrument) controller-number midi-value)) acc))))
    acc))

(defun render-program-events (time state instrument-list)
  (let ((acc '())
	(program (simple-state-program-number state))
	(bank (simple-state-program-bank state)))
    (if program
	(dolist (instrument instrument-list)
	  (let ((program-map (property instrument :program-map)))
	    (setf acc (append acc (funcall program-map time :bank bank :program program))))))
    acc))
	       
(defun render-pressure-events (time state instrument-list)
  (let ((acc '())
	(pressure (simple-state-pressure state)))
    (if pressure
	(let ((midi-value (norm->midi-data pressure)))
	  (dolist (instrument instrument-list)
	    (push (cons time (midi-channel-pressure (channel-index instrument) midi-value)) acc))))
    acc))

(labels ((playable-p (keynumber duration amplitude)
		     (not (and (rest-p keynumber)
			       (rest-p duration)
			       (rest-p amplitude))))
	 
	 (create-chord-template (chord-model chord-type keynumber inversion-degree octave)
	 			(chord-inversion (or (and (listp chord-type) chord-type)
	 					     (chord-template chord-model chord-type keynumber))
						 inversion-degree
	 					 :add-octave octave))
	
	 (expand-template (chord-model template keynumber)
	 		  (if (not (absolute-chords-p chord-model))
	 		      (mapcar #'(lambda (q)(if (not (rest-p q))
						       (+ q keynumber)
						     +rest+))
	 			      template)
	 		    template))
	 
	 (expand-chord (on-time key chord-type inversion octave articulation dynamic chord-model instrument)
		       (let ((acc '())
			     (keynumber (funcall (keynumber-map instrument) key))
			     (duration (funcall (articulation-map instrument) articulation))
			     (amplitude (funcall (dynamic-map instrument) dynamic)))
			 (if (playable-p keynumber duration amplitude)
			     (let (
				   (template (create-chord-template chord-model chord-type keynumber inversion octave))
				   (channel-index (channel-index instrument))
				   
				   (off-time (+ on-time duration))
				   (velocity (norm->midi-data amplitude))
				   )
			       (setf template (expand-template chord-model template keynumber))
			       (dolist (kn template)
				 (if (not (rest-p kn))
				     (progn
				       (push (cons on-time (midi-note-on channel-index kn velocity)) acc)
				       (push (cons off-time (midi-note-off channel-index kn 64)) acc))))
			       acc)))) )

  (defun render-note-events (on-time state chord-model instrument-list)
    (let ((acc '())
	  (key (simple-state-key state)))
      (if (and key (not (rest-p key)))
	  (let ((chord-type (simple-state-chord-type state))
		(inversion (simple-state-chord-inversion state))
		(octave (simple-state-chord-octave state))
		(articulation (simple-state-articulation state))
		(dynamic (simple-state-dynamic state)))
	    (dolist (instrument instrument-list)
	      (setf acc (append acc (expand-chord on-time key chord-type inversion octave 
						  articulation dynamic chord-model instrument))))))
      acc)))


(param *trace-simple-state-render* nil)

(defmethod render-once ((part simple-part) &key (offset 0))
  (if (not (muted-p part))
      (let* ((midi-events '())
	     (chord-model (property part :chord-model))
	     (instrument-list (property part :instruments)))
	(dolist (state (simple-part-events part))
	  (if *trace-simple-state-render*
	      (format t "~A~%" state))
	  (let* ((state-time (+ offset (simple-state-time state))))
	    (setf midi-events (append midi-events (render-bend-events state-time state instrument-list)))
	    (setf midi-events (append midi-events (render-controller-events state-time state instrument-list)))
	    (setf midi-events (append midi-events (render-program-events state-time state instrument-list)))
	    (setf midi-events (append midi-events (render-pressure-events state-time state instrument-list)))
	    (setf midi-events (append midi-events (render-note-events state-time state chord-model instrument-list)))
	    ))
	(sort-midi-events midi-events))))
		 
		 
(defmethod render-n ((part simple-part)(n integer) &key (offset 0.0))
  (let ((period (phrase-duration part))
	(template (render-once part))
	(midi-events '()))
    (dotimes (i (if (property part :render-once) 1 n))
      (dolist (event template)
	(let ((reltime (car event))
	      (message (cdr event)))
	  (push (cons (+ offset (* i period) reltime) message) midi-events))))
    (sort-midi-events midi-events)))
