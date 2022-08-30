;;;; CYCO parts xball-midi-render.lisp
;;;;

(in-package :cyco-part)

(labels ((prepare-start-times (root-time delta template)
			      (->vector (loop for i from 0 below (length template)
					      collect (+ root-time (* i delta)))))

	 
	 (prepare-end-times (start-times duration end-together)
			    (->vector (if end-together
					  (copies (length start-times)(+ duration (final start-times)))
					(loop for s in start-times collect (+ s duration)))))

	 (prepare-keylist (xb root-key)
			  (let* ((model (property xb :chord-model))
				 (ctype (next (property xb :chord-pattern))))
			    (let* ((ctemplate (if (listp ctype) ctype
						(chord-template model ctype root-key)))
				   (degree (next (property xb :inversion-pattern)))
				   (oct (next (property xb :octave-pattern)))
				   (template (chord-inversion ctemplate degree :add-octave oct))
				   (direction (next (property xb :direction-pattern))))
			      (if (not (absolute-chords-p model))
				  (setf template (loop for x in template collect (+ root-key x))))
			      (setf template (remove-if #'rest-p template))
			      (->vector (cond ((eq direction (->cyco-symbol 'up))
					       (reverse template))
					      ((eq direction (->cyco-symbol 'dice))
					       (if (> (random 1.0) 0.5)
						   (reverse template)
						 template))
					      ((eq direction (->cyco-symbol 'random))
					       (permute template))
					      (t template))))))
	 
	 (strum-chord (xb root-time root-key cindex duration dynamic)
		      (let* ((keys (prepare-keylist xb root-key))
			     (strum-delta (next (property xb :strum-pattern)))
			     (start-times (prepare-start-times root-time strum-delta keys))
			     (end-together (property xb :end-together))
			     (end-times (prepare-end-times (->list start-times) duration end-together))
			     (velocity (dynamic->velocity dynamic))
			     (acc '()))
			(dotimes (i (length keys))
			  (let ((key (aref keys i))
				(start (aref start-times i))
				(end (aref end-times i)))
			    (push (cons start (midi-note-on cindex key velocity)) acc)
			    (push (cons end (midi-note-off cindex key 64)) acc)))
			acc)) )

	(defmethod render-once ((xb xball) &key (offset 0.0) &allow-other-keys)
	  (if (muted-p xb)(return-from render-once '()))
	  (if (property xb :reset-on-repeat)
	      (reset xb)
	    (soft-reset xb))
	  (let* ((instrument (property xb :instruments))
		 (cindex (channel-index instrument))
		 (acc '())
		 (time-shift (+ offset (property xb :shift)))
		 (shuffle (property xb :shuffle-function))
		 (cuefn (property xb :cue-function)))
	    (dolist (time-spec (next (property xb :cue-cycle) :all))
	      (let* ((root-time (+ time-shift
				   (funcall cuefn xb time-spec)
				   (funcall shuffle time-spec)))
		     (root-key (funcall (keynumber-map instrument) (next (property xb :key-pattern))))
		     (articulation (next (property xb :articulation-pattern)))
		     (articulation-scale (float (if (numberp articulation)
						    1.0
						  (beat-duration xb))))
		     (duration (* articulation-scale
				  (metric-expression (funcall (articulation-map instrument) articulation))))
		     (dynamic (funcall (dynamic-map instrument)(next (property xb :dynamic-pattern)))))
		(if (notany #'rest-p (list root-key duration dynamic))
		    (dolist (event (strum-chord xb root-time root-key cindex duration dynamic))
		      (push event acc)))))
	    (sort-midi-events acc)))  )

(defmethod render-n ((xball xball)(n integer) &key (offset 0.0) &allow-other-keys)
  (reset xball)
  (setf n (if (property xball :render-once) 1 n))
  (let ((events '())
	(update (not (property xball :reset-on-repeat)))
	(template (render-once xball))
	(period (phrase-duration xball)))
    (dotimes (i n)
      (let ((start-offset (+ offset (* i period))))
	(dolist (event template)
	  (push (cons (+ start-offset (car event))(clone (cdr event))) events))
	(if update (setf template (render-once xball))) ))
  (sort-midi-events events)))
