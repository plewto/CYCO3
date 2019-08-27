;;;; test-1 orchestra
;;;;

(plugin 'general-midi)

(prune-orchestra)

(general-midi-instrument piano :program 'piano1 :channel 1)
(general-midi-instrument bass :program 'bass-acstc :channel 2)
(general-midi-instrument organ :program 'organ1 :channel 3)

(put *metronome* :channel 16)

(pass? "Instruments bound"
       (and (boundp 'piano)(instrument-p piano)
	    (boundp 'bass)(instrument-p bass)
	    (boundp 'organ)(instrument-p organ)))
	    
(pass? "Instrument channels"
       (and (eq (channel-index piano) 0)
	    (eq (channel-index bass) 1)
	    (eq (channel-index organ) 2)))

(let* ((program-map (property piano :program-map))
       (midi-events (funcall program-map 0.0 :program :default)))
  (pass? "Default piano program"
	 (and (listp midi-events)
	      (= (length midi-events) 1)
	      (let* ((event (car midi-events))
		     (time (car event))
		     (message (cdr event)))
		(and (numberp time)(zerop time)
		     (midi-program-change-p message)
		     (= (channel-index message)(channel-index piano))
		     (= (data message 0)(property piano :program-number)))))))

(let* ((program-map (property piano :program-map))
       (program-number 19)
       (midi-events (funcall program-map 0.0 :program program-number)))
  (pass? "Alternate piano program"
	 (= (data (cdr (car midi-events)) 0) program-number)))


       

