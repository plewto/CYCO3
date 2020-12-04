;;;; CYCO parts control-ghost.lisp
;;;;
;;;; A CONTROL-GHOST produces modified copies of MIDI controller events
;;;; from another part.
;;;;

(in-package :cyco-part)


(constant +control-ghost-docstring+ 
"A CONTROL-GHOST is a PART which duplicates MIDI controller events from another PART.

The original channel, controller-number, value and time may be individually
modified.

name              - Symbol, parts name.
source-part       - Part, the part to be copied.
source-controller - Integer, controller number to be copied.
source-channel    - Integer or instrument.  The MIDI channel to copy.
:out-controller   - Integer, resulting controller number, defaults to source-controller.
:out-channels     - List of MIDI channels or instruments.  Events are generated 
                    for each output channel.  For convenience a single channel/instrument
                    may be specified.  Defaults to source-channel.
:delay            - Metric expression, delay time for generated events. Default 0.
:value-map        - Function, list or vector.  Table used to map source controller 
                    values to resulting values.  List and vectors are converted 
                    to a function via WRAP.  Minus results are skipped.
                    Out of bounds results are clipped.  Defaults to #'IDENTITY.
:render-once      - Boolean., if true only do not generate events after the first
                    copy. Default nil.
:remarks          - Optional remarks text.")


(constant +control-ghost-properties+
  (append +part-properties+
	  '(:source-controller
	    :source-channel
	    :out-controller
	    :out-channels
	    :delay
	    :value-map
	    :render-once)))

(defclass control-ghost (part) nil)

(defgeneric control-ghost-p (object))
(defmethod control-ghost-p ((object t)) nil)
(defmethod control-ghost-p ((object control-ghost)) t)

(defun make-control-ghost (name source-part source-controller source-channel &key
				out-controller
				out-channels
				delay
				value-map
				render-once
				remarks)
  (let ((ghost (make-instance 'control-ghost
			      :properties +control-ghost-properties+
			      :name name
			      :remarks (->string (or remarks ""))
			      :transient t)))
    (connect source-part ghost)
    (copy-time-signature source-part ghost)
    (put ghost :source-controller source-controller)
    (put ghost :source-channel (channel source-channel :resolve))
    (put ghost :out-controller (or out-controller 1))
    (put ghost :delay (or delay 0.0))
    (put ghost :value-map (cond ((functionp value-map)
				 value-map)
				((and value-map (listp value-map))
				 (wrap value-map :out-of-bounds-value -1))
				((vectorp value-map)
				 (wrap value-map :out-of-bounds-value -1))
				(t #'identity)))
    (put ghost :render-once render-once)
    (put ghost :out-channels (mapcar #'(lambda (obj)(channel obj :resolve))
				     (if out-channels
					 (->list out-channels)
				       (->list (property ghost :source-channel)))))
    (reset ghost)
    ghost))

(setf (documentation 'make-control-ghost 'function) +control-ghost-docstring+)


(defmacro control-ghost (name source-part source-controller source-channel
			      &key
			      out-controller
			      out-channels
			      delay
			      value-map
			      render-once
			      remarks)
  "CONTROL-GHOST is identical to MAKE-CONTROL-GHOST except that it binds the 
new part object to the symbol name."
  `(progn
     (part-banner (name (parent ,source-part)) ',name)
     (let ((ghost (make-control-ghost ',name ,source-part
				      ,source-controller ,source-channel
				      :out-controller ,out-controller
				      :out-channels ,out-channels
				      :delay ,delay
				      :value-map ,value-map
				      :render-once ,render-once
				      :remarks ,remarks)))
       (defparameter ,name ghost)
       ghost)))
			       

(defmethod reset ((ghost control-ghost))
  (reset (parent ghost))
  ghost)


(defmethod clone ((mother control-ghost) &key new-name new-parent)
  (let* ((name-format (or new-name "~A"))
	 (name (sformat name-format (name mother)))
	 (parent (or new-parent (parent mother)))
	 (daughter (make-control-ghost name parent
				    (property mother :source-controller)
				    (property mother :source-channel)
				    :out-controller (property mother :out-controller)
				    :out-channels (property mother :out-channels)
				    :delay (property mother :delay)
				    :value-map (property mother :value-map)
				    :remarks (remarks mother))))
    daughter))



(labels ((generate-source-events (ghost offset)
	    (let ((source-part (clone (parent ghost) :new-name "TEMP")))
	      (put source-part :mute nil)
	      (reset source-part)
	      (prog1
		  (render-once source-part :offset offset)
		(disconnect source-part)))) )

  (defmethod render-once ((ghost control-ghost) &key (offset 0.0))
    (if (muted-p ghost)(return-from render-once '()))
    (let ((midi-events '())
	  (delay (let* ((n (property ghost :delay))
			(scale (if (numberp n)
				   1.0
				 (float (beat-duration ghost)))))
		   (* scale (metric-expression n))))
	  (source-channel-index (1- (property ghost :source-channel)))
	  (source-controller (property ghost :source-controller))
	  (map-function (property ghost :value-map))
	  (out-channel-index-list (mapcar #'(lambda (c)(1- c))(property ghost :out-channels)))
	  (out-controller (property ghost :out-controller)))
      (dolist (event (generate-source-events ghost offset))
	(let* ((time (+ (car event) delay))
	       (message (cdr event)))
	  (if (and (midi-control-change-p message)
		   (= (data message 0) source-controller)
		   (= (channel-index message) source-channel-index))
	      (let ((value (funcall map-function (data message 1))))
		(dolist (ci out-channel-index-list)
		  (if (not (minusp value))
		      (push (cons time (midi-control-change ci out-controller (limit value 0 127)))
			    midi-events)))))))
      (sort-midi-events midi-events))))
  

(defmethod render-n ((ghost control-ghost)(n integer) &key (offset 0.0))
  (reset ghost)
  (let ((template (render-once ghost))
	(delay (property ghost :delay))
	(midi-events '())
	(period (phrase-duration ghost)))
    (dotimes (i (if  (property ghost :render-once) 1 n))
      (let ((time-shift (+ delay (* i period) offset)))
	(dolist (event template)
	  (let ((time (+ time-shift offset (car event)))
		(message (clone (cdr event))))
	    (push (cons time message) midi-events)))))
    (sort-midi-events midi-events)))	 
