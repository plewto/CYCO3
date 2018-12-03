;;;; CYCO
;;;; Strummer part methods.
;;;;

(labels (

	 (process-touch
	  (acc time state cindex)
	  (let ((touch (strummer-state-touch state)))
	    (if touch
		(cons (cons time (midi-channel-pressure cindex (norm->midi-data touch)))
		      acc)
	      acc)))

	 (process-bend
	  (acc time state cindex)
	  (let ((bend (strummer-state-bend state)))
	    (if bend
		(let* ((data (bend->midi-data bend))
		       (lsb (aref data 0))
		       (msb (aref data 1)))
		  (cons (cons time (midi-pitch-bend cindex lsb msb)) acc))
	      acc)))

	 (process-controller
	  (acc time state cindex)
	  (let ((ctrl (strummer-state-controller-number state))
		(cc (strummer-state-controller-value state)))
	    (if (and ctrl cc)
		(let ((data (norm->midi-data cc)))
		  (cons (cons time (midi-control-change cindex ctrl data)) acc))
	      acc)))
	  
	 (process-program
	  (acc time state instrument)
	  (let ((program (strummer-state-program-number state)))
	    (if program 
		(let ((bank (strummer-state-program-bank state))
		      (pmap (property instrument :program-map)))
		  (setf acc (append acc (funcall pmap time :bank bank :program program))))
	      acc)))
	 
	 (simple-note-events
	  (time channel-index keynumber velocity duration)
	  (list (cons time (midi-note-on channel-index keynumber velocity))
		(cons (+ time duration)
		      (midi-note-off channel-index keynumber 0))))

	 (process-grace-note
	  (acc time state instrument)
	  (let ((grace-key (strummer-state-grace-key state)))
	    (if grace-key
		(let ((kn (funcall (keynumber-map instrument) grace-key))
		      (d (funcall (articulation-map instrument)
				  (strummer-state-grace-articulation state)))
		      (velocity (norm->midi-data
				 (funcall (dynamic-map instrument)
					  (* (value (strummer-state-dynamic state))
					     (strummer-state-grace-amp-scale state))))) )
		  (if (not (rest-p kn))
		      (let ((ci (channel-index instrument)))
			(append acc (list (cons time (midi-note-on ci kn velocity))
					  (cons (+ time d)(midi-note-off ci kn 0)))))
		    acc))
	      acc)))

	 (expand-chord-template
	  (state chord-model base-key instrument)
	  (let ((kn (funcall (keynumber-map instrument) base-key)))
	    (if (rest-p kn)(return-from expand-chord-template '(r)))
	    (let* ((ctype (strummer-state-chord-type state))
		   (template (if (listp ctype)
				 ctype
			       (chord-template chord-model ctype kn))))
	      (if (or (listp ctype)(not (absolute-chords-p chord-model)))
		  (let ((bcc '()))
		    (dolist (offset template)
		      (if (not (rest-p offset))
			  (push (+ offset kn) bcc)))
		    (setf template (reverse bcc))))
	      (chord-inversion
	       template
	       (strummer-state-chord-inversion state)
	       :add-octave (strummer-state-chord-octave state)))))

	 (expand-strum-times
	  (start-time chord-template state)
	  (let ((time start-time)
		(delay (strummer-state-strum-delay state))
		(scale (strummer-state-strum-acceleration state))
		(bcc '()))
	    (dotimes (i (length chord-template))
	      (push time bcc)
	      (setf time (+ time delay))
	      (setf delay (* delay scale)))
	    (->vector (reverse bcc))))

	 ;; start-times as vector
	 ;;
	 (expand-staggered-end-times
	  (start-times duration)
	  (let ((bcc '()))
	    (dotimes (i (length start-times))
	      (push (+ (aref start-times i) duration) bcc))
	    (->vector (reverse bcc))))

	 ;; start-times as vector
	 ;;
	 (expand-homogeneous-end-times
	  (start-times duration)
	  (let ((end-time (+ (apply #'max (->list start-times)) duration)))
	    (->vector (copies (length start-times) end-time))))

	 ;; start times as vector
	 ;; return vector
	 ;;
	 (expand-end-times
	  (start-times duration state)
	  (if (strummer-state-strum-end-together state)
	      (expand-homogeneous-end-times start-times duration)
	    (expand-staggered-end-times start-times duration)))

	 (expand-dynamics
	  (state count base-amp)
	  (let ((bcc '())
		(ascale (strummer-state-strum-amp-scale state))
		(amp base-amp))
	    (dotimes (i count)
	      (push amp bcc)
	      (setf amp (limit (* amp ascale) 0 1)))
	    (->vector (reverse bcc))))

	 ;; chord-template as list
	 ;; returns vector
	 (select-strum-direction
	  (state chord-template)
	  (let* ((dir (next-1 (strummer-state-strum-direction state)))
		 (rs (cond ((eq dir 'down)
			    chord-template)
			   ((eq dir 'up)
			    (reverse chord-template))
			   ((eq dir 'dice)
			    (if (> (random 1.0) 0.5)
				chord-template
			      (reverse chord-template)))
			   ((eq dir 'random)
			    (permute chord-template))
			   (t
			    (cyco-error
			     (sformat "Invalid STRUMMER STRUM-DIRECTION ~A" dir))))))
			     
	    (->vector rs)))
	 
	 (strum-chord
	  (state start-time channel-index chord-template duration base-amp)
	  (let* ((count (length chord-template))
		 (note-on-times (expand-strum-times start-time chord-template state))
		 (note-off-times (expand-end-times note-on-times duration state))
		 (dynamics (expand-dynamics state count base-amp))
		 (note-list (select-strum-direction state chord-template))
		 (bcc '()))
	    (dotimes (i count)
	      (let ((kn (aref note-list i)))
		(if (not (rest-p kn))
		    (let ((on-time (aref note-on-times i))
			  (off-time (aref note-off-times i))
			  (velocity (norm->midi-data (aref dynamics i))))
		      (push (cons on-time (midi-note-on channel-index kn velocity)) bcc)
		      (push (cons off-time (midi-note-off channel-index kn 0)) bcc)))))
	    (reverse bcc)))
	  
	 
	 (process-key-events
	  (acc time state chord-model instrument)
	  ;; (format t "------------------------------------- PROCESS-KEY-EVENT~%") ;; DEBUG
	  (let* ((cindex (channel-index instrument))
		 (base-key (let ((bk (strummer-state-key state)))
	  		     (if (or (null bk)(rest-p bk))
	  			 (return-from process-key-events acc))
	  		     bk))
		 (chord-template (expand-chord-template state chord-model base-key instrument))
		 (base-amp (funcall (dynamic-map instrument)
				    (approximate (next-1 (strummer-state-dynamic state))
						 :scale (strummer-state-dynamic-blur state)
						 :max (strummer-state-dynamic-max state)
						 :min (strummer-state-dynamic-min state))))
		 (base-dur (funcall (articulation-map instrument)
				    (or (strummer-state-articulation state) 1.0))))
	    (setf acc (append acc (strum-chord state time cindex chord-template base-dur base-amp))))
	  acc)

	 ) ;; END LABELS ASSIGNMENTS

  (defmethod render-once ((part strummer) &key (offset 0))
    (let* ((acc '())
  	   (chord-model (property part :chord-model))
  	   (instrument (property part :instruments))
  	   (cindex (channel-index instrument)))
      (dolist (state (strummer-events part))
      	(let ((time (+ offset (or (strummer-state-time state) 0))))
      	  (setf acc (process-touch acc time state cindex))
      	  (setf acc (process-bend acc time state cindex))
      	  (setf acc (process-controller acc time state cindex))
      	  (setf acc (process-program acc time state instrument))
      	  (setf acc (process-grace-note acc time state instrument))
      	  (setf acc (process-key-events acc time state chord-model instrument))))
      acc))
      

  ) ;; END LABELS


	 
			  
(defmethod render-n ((part strummer)(n integer) &key (offset 0.0))
  (let ((period (phrase-duration part))
	(template (render-once part))
	(acc '()))
    (dotimes (i (if (property part :render-once) 1 n))
      (dolist (evn template)
	(let ((reltime (car evn))
	      (msg (cdr evn)))
	  (push (cons (+ offset (* i period) reltime) msg) acc))))
    (sort-midi-events acc)))



