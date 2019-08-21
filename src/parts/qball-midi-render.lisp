;;;; CYCO qball midi-render
;;;;

(in-package :cyco-part)

(labels ((render-event
	  (qball time instrument-list key-list articulation dynamic)
	  (if (or (rest-p articulation)(rest-p dynamic))
	      (return-from render-event nil))
	  (let ((midi-events '())
		(articulation-scale (if (numberp articulation)
					1.0
				      (beat-duration qball))))
	    (dolist (instrument instrument-list)
	      (let* ((channel-index (channel-index instrument))
		     (keynumber-map (property instrument :keynumber-map))
		     (articulation-map (property instrument :articulation-map))
		     (dynamic-map (property instrument :dynamic-map))
		     (duration (* (funcall articulation-map articulation) articulation-scale))
		     (end-time (+ time duration))
		     (amp (funcall dynamic-map dynamic)))
		(dolist (k key-list)
		  (let* ((keyspec (funcall keynumber-map k))
			 (keynumber (if (listp keyspec)(car keyspec) keyspec)))
		    (if (not (or (rest-p keynumber)
				 (rest-p duration)
				 (rest-p amp)))
			(let ((velocity (norm->midi-data amp))) 
			  (push (cons time (midi-note-on channel-index keynumber velocity)) midi-events)
			  (push (cons end-time (midi-note-off channel-index keynumber 0)) midi-events)))))))
	    midi-events)))

  (defmethod render-once ((qball qball) &key (offset 0.0))
    (if (not (muted-p qball))
	(progn
	  (if (property qball :reset-on-repeat)
	      (reset qball)
	    (soft-reset qball))
	  (let ((midi-events '())
		(shuffle-function (property qball :shuffle-function))
		(cuefn (property qball :cue-function)))
	    (dolist (time-spec (next (property qball :cue-cycle) :all))
	      (let ((time (+ offset
			     (funcall cuefn qball time-spec)
			     (funcall shuffle-function time-spec)))
		    (keylist (->list (next (property qball :key-pattern))))
		    (articulation (next (property qball :articulation-pattern)))
		    (dynamic (next (property qball :dynamic-pattern)))
		    (instrument-list (->list (next (property qball :instruments)))) )
		(setf midi-events (append midi-events (render-event qball time instrument-list keylist articulation dynamic)))))
	    (dolist (sub-parts (reverse (children qball)))
	      (setf midi-events (append midi-events (render-once sub-parts :offset offset))))
	    (sort-midi-events midi-events))))))

(defmethod render-n ((qball qball)(n integer) &key (offset 0.0))
  (reset qball)
  (let ((period (phrase-duration qball))
	(midi-events '()))
    (dotimes (i (if (property qball :render-once) 1 n))
      (dolist (event (render-once qball))
	(let ((relative-time (car event))
	      (message (cdr event)))
	  (push (cons (+ offset (* i period) relative-time) (clone message)) midi-events))))
    (sort-midi-events midi-events)))



