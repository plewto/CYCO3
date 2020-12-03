;;;; CYCO key-ghost
;;;;


(in-package :cyco-part)

(constant +key-ghost-properties+
  (append +part-properties+
	  '(:source-channel
	    :out-channels
	    :delay
	    :key-map
	    :velocity-map)))

(defclass key-ghost (part) nil)

(defgeneric key-ghost-p (object))
(defmethod key-ghost-p ((object t)) nil)
(defmethod key-ghost-p ((object key-ghost)) t)

(defun make-key-ghost (name source-part source-channel &key
			    out-channels
			    delay
			    key-map
			    velocity-map
			    render-once
			    remarks)
  (let ((ghost (make-instance 'key-ghost
			      :properties +key-ghost-properties+
			      :name name
			      :remarks (->string (or remarks ""))
			      :transient t)))
    (connect source-part ghost)
    (copy-time-signature source-part ghost)
    (put ghost :source-channel (channel (or source-channel 1) :resolve))
    (put ghost :out-channels (mapcar #'(lambda (obj)(channel obj :resolve))
				     (if out-channels
					 (->list out-channels)
				       (->list (property ghost :source-channel)))))
    (put ghost :delay (or delay 0.0))
    (put ghost :key-map (cond ((functionp key-map)
			       key-map)
			      ((and key-map (or (listp key-map)(vectorp key-map)))
			       (wrap key-map :out-of-bounds-value -1))
			      (t #'identity)))
    (put ghost :velocity-map (cond ((functionp velocity-map)
				    velocity-map)
				   ((and velocity-map (or (listp velocity-map)
							  (vectorp velocity-map)))
				    (wrap velocity-map :out-of-bounds-value -1))
				   (t #'identity)))
    (put ghost :render-once render-once)
    (reset ghost)
    ghost))

(defmacro key-ghost (name source-part source-channel &key
			  out-channels
			  delay
			  key-map
			  velocity-map
			  render-once
			  remarks)
  `(progn
     (part-banner (name (parent ,source-part)) ',name)
     (let ((ghost (make-key-ghost ',name ,source-part ,source-channel
				  :out-channels ,out-channels
				  :delay ,delay
				  :key-map ,key-map
				  :velocity-map ,velocity-map
				  :render-once ,render-once
				  :remarks ,remarks)))
       (defparameter ,name ghost)
       ghost)))


(defmethod clone ((mother key-ghost) &key new-name new-parent)
  (let* ((name-format (or new-name "~A"))
	 (name (sformat name-format (name mother)))
	 (parent (or new-parent (parent mother)))
	 (daughter (make-key-ghost name parent
				   (property mother :source-channel)
				   :out-channels (property mother :out-channels)
				   :delay (property mother :delay)
				   :key-map (property mother :key-map)
				   :velocity-map (property mother :velocity-map)
				   :render-once (property mother :render-once)
				   :remarks (remarks mother))))
    daughter))


(labels ((resolve-delay-time (ghost)
	    (let* ((dly (property ghost :delay))
		   (scale (if (numberp dly)
			      1.0
			    (float (beat-duration ghost)))))
	      (* scale (metric-expression dly))))

	 (is-key-message (msg)
	    (or (midi-note-on-p msg)
		(midi-note-off-p msg)))

	 (filter-events (source-list target-index)
	    (remove-if-not #'(lambda (evnt)
			       (let ((msg (cdr evnt)))
				 (and (is-key-message msg)
				      (= (channel-index msg) target-index))))
			   source-list))
	 
	 (generate-source-events (ghost target-index offset)
	    (let ((source-part (clone (parent ghost) :new-name "TEMP ~A")))
	      (reset source-part)
	      (prog1
		  (filter-events (render-once source-part :offset offset) target-index)
		(disconnect source-part)))) )

  (defmethod render-once ((ghost key-ghost) &key (offset 0.0))
    (if (muted-p ghost)(return-from render-once '()))
    (let* ((midi-events '())
	   (target-index (1- (property ghost :source-channel)))
	   (out-indexes (mapcar #'1- (property ghost :out-channels)))
	   (delay (resolve-delay-time ghost))
	   (kmap (property ghost :key-map))
	   (vmap (property ghost :velocity-map))
	   (source-events (generate-source-events ghost offset target-index)))
      (dolist (event source-events)
	(let* ((time (+ delay (car event)))
	       (msg (cdr event))
	       (key (funcall kmap (data msg 0)))
	       (velocity (funcall vmap (data msg 1))))
	  (if (or (midi-note-off-p msg)
		  (and (<= 0 key)(<= 0 velocity)))
	      (dolist (ci out-indexes)
		(push (cons time (if (midi-note-on-p msg)
				     (midi-note-on ci key velocity)
				   (midi-note-off ci key 64)))
		      midi-events)))))
      (sort-midi-events midi-events))) )