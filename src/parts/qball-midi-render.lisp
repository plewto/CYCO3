;;;; CYCO parts qball-midi-render.lisp
;;;;
;;;; Defines rendering methods for QBALL.
;;;;

(in-package :cyco-part)

(labels ((render-event
	  (qball time instrument-list key-list articulation dynamic)
	  
	  (if (or (rest-p articulation)(rest-p dynamic))
	      (return-from render-event nil))
	  (let ((midi-events '())
		(articulation-scale (float (if (numberp articulation)
					       1.0
					     (beat-duration qball)))))
	    (dolist (instrument instrument-list)
	      (let* ((channel-index (channel-index instrument))
		     (keynumber-map (property instrument :keynumber-map))
		     (articulation-map (property instrument :articulation-map))
		     (dynamic-map (property instrument :dynamic-map))
		     (duration (* articulation-scale
				  (metric-expression 
				   (funcall articulation-map articulation))))
		     (end-time (+ time duration))
		     (amp (funcall dynamic-map dynamic)))
		(dolist (k key-list)
		  (let* ((keyspec (funcall keynumber-map k))
			 (keynumber (if (listp keyspec)(car keyspec) keyspec)))
		    (if (not (or (rest-p keynumber)
				 (rest-p duration)
				 (rest-p amp)))
			(let ((velocity (norm->midi-data amp)))
			  (push (cons time
				      (midi-note-on channel-index
						    keynumber
						    velocity))
				midi-events)
			  (push (cons end-time
				      (midi-note-off channel-index
						     keynumber 0))
				midi-events)))))))
	    midi-events)))
 
  (defmethod render-once ((qball qball) &key (offset 0.0) &allow-other-keys)
      (if (muted-p qball)(return-from render-once '()))
      (if (property qball :reset-on-repeat)
	  (reset qball)
	(soft-reset qball))
      (let ((midi-events '())
	    (time-shift (+ offset (property qball :shift)))
	    (shuffle-function (property qball :shuffle-function))
	    (cuefn (property qball :cue-function)))
	(dolist (time-spec (next (property qball :cue-cycle) :all))
	  (let ((time (+ time-shift
			 (funcall cuefn qball time-spec)
			 (funcall shuffle-function time-spec)))
		(keylist (->list (next (property qball :key-pattern))))
		(articulation (next (property qball :articulation-pattern)))
		(dynamic (next (property qball :dynamic-pattern)))
		(instrument-list (->list (next (property qball :instruments)))) )
	    (setf midi-events (append midi-events 
				      (render-event qball 
						    time 
						    instrument-list 
						    keylist 
						    articulation 
						    dynamic)))))
	(sort-midi-events midi-events))) )

(defmethod render-n ((qball qball)(n integer) &key (offset 0.0) &allow-other-keys)
  (reset qball)
  (setf n (if (property qball :render-once) 1 n))
  (let ((events '())
	(update (not (property qball :reset-on-repeat)))
	(template (render-once qball))
	(period (phrase-duration qball)))
    (dotimes (i n)
      (let ((start-offset (+ offset (* i period))))
	(dolist (event template)
	  (push (cons (+ start-offset (car event))(clone (cdr event))) events))
	(if update (setf template (render-once qball))) ))
  (sort-midi-events events)))
