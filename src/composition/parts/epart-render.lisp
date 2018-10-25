;;;; CYCO3 src/composition/parts/epart-render
;;;;

(labels ((generate-touch-events
	  (time touch cindex-list)
	  (let ((bcc '())
		(value (norm->midi-data touch)))
	    (dolist (ci cindex-list)
	      (push (cons time (midi-channel-pressure ci value)) bcc))
	    bcc))

	 (generate-bend-events
	  (time bend cindex-list)
	  (let* ((bcc '())
		 (data (bend->midi-data bend))
		 (lsb (aref data 0))
		 (msb (aref data 1)))
	    (dolist (ci cindex-list)
	      (push (cons time (midi-pitch-bend ci lsb msb)) bcc))
	    bcc))

	 (generate-controller-events
	  (time ctrl value cindex-list)
	  (let ((bcc '())
		(data (norm->midi-data value)))
	    (dolist (ci cindex-list)
	      (push (cons time (midi-control-change ci ctrl data)) bcc))
	    bcc))
		
	 (generate-program-change-events
	  (time bank program instruments)
	  (setf bank (if (eq bank 'default) :default bank))
	  (setf program (if (eq program 'default) :default program))
	  (let ((bcc '()))
	    (dolist (instrument instruments)
	      (let ((pmap (property instrument :program-map)))
		(dolist (evn (funcall pmap time :bank bank :program program))
		  (push evn bcc))))
	    bcc)) 

	 (simple-note-events
	  (time channel-index keynumber velocity duration)
	  (list (cons time (midi-note-on channel-index keynumber velocity))
		(cons (+ time duration)
		      (midi-note-off channel-index keynumber 0))))

	 (generate-grace-notes
	  (time key amp dur instruments)
	  (let ((bcc '()))
	    (dolist (instrument (->list instruments))
	      (let (kn d a)
	 	(setf kn (funcall (keynumber-map instrument) key))
	 	(setf d (funcall (articulation-map instrument) dur))
	 	(setf a (funcall (dynamic-map instrument) amp))
	 	(if (every #'(lambda (q)(not (rest-p q)))(list kn d a))
	 	    (let ((ci (channel-index instrument))
	 		  (vel (norm->midi-data a)))
	 	      (push (cons time (midi-note-on ci kn vel)) bcc)
	 	      (push (cons (+ time d)(midi-note-off ci kn 0)) bcc)))))
	    bcc))
	      
	 ;; template, start-times and stop-times are simple vectors of identical length
	 (strum-chord
	  (root template start-times stop-times channel-index amp amp-scale)
	  (let ((bcc '()))
	    (dotimes (i (length template))
	      (let* ((offset (aref template i))
		     (key (let ((k (+ root offset)))
			    (while (minusp k)(setf k (+ k 12)))
			    (while (> k 127)(setf k (- k 12)))
			    k))
		     (start (aref start-times i))
		     (stop (aref stop-times i))
		     (duration (- stop start))
		     (velocity (norm->midi-data amp)))
		(dolist (evn (simple-note-events start channel-index key velocity duration))
		  (push evn bcc))
		(setf amp (* amp amp-scale))))
	    bcc))
	      
	 (expand-strum-times
	  (count start delay acceleration)
	  (let ((bcc '())
		(time start)
		(dscale (/ 1.0 acceleration)))
	    (dotimes (i count)
	      (push time bcc)
	      (setf time (max 0 (+ time delay)))
	      (setf delay (* delay dscale)))
	    (->vector (reverse bcc))))

	 (expand-staggered-end-times
	  (start-times duration)
	  (let ((bcc '()))
	    (dolist (start (->list start-times))
	      (push (+ start duration) bcc))
	    (->vector (reverse bcc))))

	 (expand-homogeneous-end-times
	  (start-times duration)

	  (let* ((count (length start-times))
		 (max-start (apply #'max (->list start-times)))
		 (end-time (+ max-start duration)))
	    (->vector (copies count end-time))))

	 (expand-end-times
	  (start-times duration end-together)
	  (if end-together
	      (expand-homogeneous-end-times start-times duration)
	    (expand-staggered-end-times start-times duration))) )

  (defmethod render-once ((epart epart) &key (offset 0))
    (let* ((acc '())
	   (instrument-pattern (property epart :instruments))
	   (instrument-list (->list (next instrument-pattern :all)))
	   (cindex-list (mapcar #'channel-index instrument-list)))
      (dolist (state (epart-events epart))
	(let ((time (+ offset (epart-state-time state))))
	  (let ((touch (epart-state-touch state)))
	    (if touch
		(setf acc (append acc (generate-touch-events time
							     touch
							     cindex-list)))))
	  (let ((bend (epart-state-bend state)))
	    (if bend
		(setf acc (append acc (generate-bend-events time
							    bend
							    cindex-list)))))
	  (let ((ctrl (epart-state-controller-number state))
		(value (epart-state-controller-value state)))
	    (if (and ctrl value)
		(setf acc (append acc (generate-controller-events time 
								  ctrl
								  value
								  cindex-list)))))
	  (let ((program (epart-state-program-number state))
		(bank (epart-state-program-bank state)))
	    (if (or bank program)
		(setf acc (append acc (generate-program-change-events time
								      bank
								      program
								      instrument-list)))))
	  (let ((grace-key (epart-state-grace-key state)))
	    (if grace-key
		(let ((delay (metric-expression (or (epart-state-grace-delay state) 0.05)))
		      (amp (* (or (epart-state-grace-amp-scale state) 0.5)
			      (value (epart-state-dynamic state))))
		      (dur (or (epart-state-grace-articulation state) 0.1)))
		  (setf acc (append acc (generate-grace-notes (+ time delay)
		  					      grace-key amp dur
		  					      instrument-list)))
		  )))
	  
	  ;; Process key/chord events
	  (block keyblock
	    (let ((base-key (epart-state-key state)))            
	      (if base-key
		  (let* ((base-amp (approximate (next (epart-state-dynamic state))
						:scale (epart-state-dynamic-blur state)
						:min (epart-state-dynamic-min state)
						:max (epart-state-dynamic-max state)))
			 (base-dur (or (epart-state-articulation state) 1.0))
			 (base-chord (chord-inversion
				      (epart-state-chord-template state)
				      (epart-state-chord-inversion state)
				      :add-octave (epart-state-chord-octave state)))
			 (strum-start-times (let ((delay (epart-state-strum-delay state))
						  (delay* (epart-state-strum-acceleration state)))
					      (expand-strum-times (length base-chord)
								  time delay delay*))) )
		    (block instrument-block
		      (dolist (instrument (->list (next instrument-pattern)))
			(let* ((root-key (let ((kn (funcall (keynumber-map instrument)
							    base-key)))
					   (if (rest-p kn)
					       (return-from instrument-block nil)
					     kn)))

			       (dur (let* ((d1 (cond ((rest-p base-dur)
						      (return-from instrument-block))
						     ((numberp base-dur)
						      base-dur)
						     (t
						      (* (metric-expression base-dur)
							   (beat-duration epart)))))
					   (d2 (funcall (dynamic-map instrument) d1)))
				      (if (rest-p d2)
					  (return-from instrument-block))
				      d2))
					
			       
			       (amp (let ((a (funcall (dynamic-map instrument)
						      base-amp)))
				      (if (<= a 0)
					  (return-from instrument-block a)
					a)))
			       (amp* (or (epart-state-strum-amp-scale state) 1.0))
			       (strum-stop-times (expand-end-times
						  strum-start-times dur
						  (epart-state-strum-end-together state)))
			       (local-chord (let* ((pat (epart-state-strum-direction state))
						   (dir (next pat)))
					      (cond ((eq dir 'down)
						     (->vector base-chord))
						    ((eq dir 'up)
						     (->vector (reverse base-chord)))
						    ((eq dir 'dice)
						     (if (> (random 1.0) 0.5)
							 base-chord
						       (->vector (reverse base-chord))))
						    ((eq dir 'random)
						     (->vector (permute base-chord)))
						    (t (->vector base-chord)))))
			       (events (strum-chord root-key local-chord
						    strum-start-times strum-stop-times
						    (channel-index instrument)
						    amp amp*)))
			  (setf acc (append acc events)))))))))))
      (dolist (c (reverse (children epart)))
	(setf acc (append acc (render-once c :offset offset))))
      (sort-midi-events acc))))
			  
(defmethod render-n ((part epart)(n integer) &key (offset 0.0))
  (let ((period (phrase-duration part))
	(template (render-once part))
	(acc '()))
    (dotimes (i (if (property part :render-once) 1 n))
      (dolist (evn template)
	(let ((reltime (car evn))
	      (msg (cdr evn)))
	  (push (cons (+ offset (* i period) reltime) msg) acc))))
    (sort-midi-events acc)))

