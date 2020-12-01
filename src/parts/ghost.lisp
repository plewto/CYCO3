;;;; CYCO Ghost
;;;;

(in-package :cyco-part)

(constant +ghost-properties+
	  (append +part-properties+
		  '(:source
		    :shift
		    :render-once
		    :delay
		    :transpose-steps
		    :pivot
		    :ampscale
		    :keytable
		    :track-channel
		    :output-channel)))

(defclass ghost (Part) nil
  (:documentation
   "A GHOST is a PART which tracks the note events of another part.
The ghosted notes may be re-channelized, delayed, transposed, inverted,
velocity scaled and key-mapped.

A potential source of confusion is the time-signature of the Ghost.  The
ghost may only be a child of a section, and thus inherits the
time-signature of the section and not that of the ghosted part.  Delay
times are specified in terms of the section time-signature. 

The tracked part may have a different time-signature, specifically it may
have a lower bar count.  When the ghost part is rendered it uses the
tracked parts duration, and -not- the section duration.  As a consequence
the Ghost phrase-duration method returns the duration of the tracked part
and -not- the normally expected value of the nominal time-signature.

A Ghost may not have child nodes."))




(let ((docstring
       "Create new GHOST part.
name       - Symbol
source     - Part to be tracked.
track      - Instrument or MIDI channel to track.
             A part may contain multiple channels, a Ghost however only
	     tracks a single channel from the part.
:delay     - Ghosted events delay time.  The delay is specified in terms
             of the parent Section's time-signature and cue-function.
:transpose - Transposition of ghosted notes.
:pivot     - Pivot for key inversion
:keytable  - A vector of length 128 which maps keynumbers.  A value of -1 
             produces a rest.  Defaults to +DEFAULT-KEYTABLE+
:ampscale  - Amplitude scaling factor, default 1.0
:outchan   - Ghosted note MIDI channel, defaults to the track channel.
:remarks   - 

Key number manipulation function is the composition:

    keytable[invert(transpose(key))]"))


  (defun make-ghost (name source-part track &key
			  (delay nil)
			  (transpose nil)
			  (pivot nil)
			  (ampscale 1.0)
			  (keytable +default-keytable+)
			  (outchan nil)
			  (remarks ""))
    docstring
    (if (not (part-p source-part))
	(progn
	  (cyco-type-error 'make-ghost 'part source-part
			   (sformat "Source-Part argument to MAKE-GHOST must be a PART."))
	  (return-from make-ghost)))
    (let* ((parent-section (parent source-part))
	   (new-ghost (make-instance 'ghost
				:properties +ghost-properties+
				:name name
				:remarks (->string (or remarks ""))
				:transient t)))
      (connect parent-section new-ghost)
      (copy-time-signature parent-section new-ghost)
      (put new-ghost :source source-part)
      (put new-ghost :delay (or delay 0.0))
      (put new-ghost :transpose-steps transpose)
      (put new-ghost :pivot pivot)
      (put new-ghost :ampscale (or ampscale 1.0))
      (put new-ghost :keytable (or keytable +default-keytable+))
      (put new-ghost :track-channel (channel track))
      (put new-ghost :output-channel (or (and outchan (channel outchan :resolve))
				     (channel track)))
      (put new-ghost :transposable nil)
      (put new-ghost :reversible nil)
      new-ghost)))



(defmacro ghost (name source-part track &key
		      (delay nil)
		      (transpose nil)
		      (pivot nil)
		      (ampscale 1.0)
		      (keytable +default-keytable+)
		      (outchan nil)
		      (remarks ""))
  "The GHOST macro is identical to the MAKE-GHOST except it binds the new ghost part to name."
  `(progn
    (part-banner (name (parent ,source-part)) ',name)
    (let ((new-ghost (make-ghost ',name ,source-part ,track
			    :delay ,delay
			    :transpose ,transpose
			    :pivot ,pivot
			    :ampscale ,ampscale
			    :keytable ,keytable
			    :outchan ,outchan
			    :remarks ,remarks)))
      (defparameter ,name new-ghost)
      new-ghost)))

(defmethod connect ((parent ghost)(child t))
  "Ghost objects are not allowed to have child nodes."
  (cyco-type-error 'connect 'cyco-node parent
		   (sformat "GHOST objects may not have child nodes.")))

(defmethod reset ((ghost ghost))
  "Calls reset on the tracked part."
  (reset (property ghost :source)))

(defmethod clone ((source-ghost ghost) &key new-name new-parent)
  (let* ((name-format (or new-name "~A"))
	 (name (->symbol (sformat name-format (name source-ghost))))
	 (parent (or new-parent (parent source-ghost)))
	 (source (clone (property source-ghost :source) :new-name name-format :new-parent parent))
	 (new-ghost (make-ghost name source (property source-ghost :track-channel)
			   :delay (property source-ghost :delay)
			   :transpose (property source-ghost :transpose-steps)
			   :pivot (property source-ghost :pivot)
			   :ampscale (property source-ghost :ampscale)
			   :keytable (property source-ghost :keytable)
			   :outchan (property source-ghost :output-channel)
			   :remarks (remarks source-ghost))))
    (copy-time-signature source-ghost new-ghost)
    new-ghost))

(labels ((allow (message channel-index)
		(and (or (midi-note-on-p message)
			 (midi-note-off-p message))
		     (= (channel-index message) channel-index)))
	 
	 (key-wrangle (key-number ktab transpose-amount inversion-pivot-key)
		      (let ((key-number2 (invert (transpose key-number transpose-amount) inversion-pivot-key)))
			(if (and (<= 0 key-number2)(< key-number2 128))
			    (aref ktab key-number2)
			  -1)))
	 
	 (amp-wrangle (velocity ampscale)
		      (let ((v2 (truncate (* ampscale velocity))))
			(limit v2 0 127))) )

  (defmethod render-once ((ghost ghost) &key (offset 0.0))
    (let ((midi-events '())
	  (delay (let* ((n (property ghost :delay))
			(scale (if (numberp n)
				   1
				 (beat-duration ghost))))
		   (* scale (metric-expression n))))
	  (transpose-amount (property ghost :transpose-steps))
	  (inversion-pivot-key (property ghost :pivot))
	  (key-table (property ghost :keytable))
	  (ampscale (property ghost :ampscale))
	  (channel-index-track (1- (property ghost :track-channel)))
	  (channel-index-out (1- (property ghost :output-channel))))
      (dolist (event (render-once (property ghost :source) :offset offset))
	(let ((time (car event))
	      (message (cdr event)))
	  (if (allow message channel-index-track)
	      (let ((key-number (key-wrangle (data message 0) key-table transpose-amount inversion-pivot-key))
		    (velocity (amp-wrangle (data message 1) ampscale)))
		(if (not (rest-p key-number))
		    (push (cons (+ time delay)
				(if (midi-note-on-p message)
				    (midi-note-on channel-index-out key-number velocity)
				  (midi-note-off channel-index-out key-number velocity)))
			  midi-events))))))
      (sort-midi-events midi-events))) )


(defmethod render-n ((ghost ghost)(n integer) &key (offset 0.0))
  (reset ghost)
  (let ((template (render-once ghost))
	(delay (property ghost :delay))
	(shift (or (property ghost :shift) 0))
	(midi-events '())
	(period (phrase-duration (property ghost :source))))
    (dotimes (i (if  (property ghost :render-once) 1 n))
      (let ((time-shift (+ delay shift (* i period) offset)))
	(dolist (event template)
	  (let ((time (+ time-shift (car event)))
		(message (clone (cdr event))))
	    (push (cons time message) midi-events)))))
    (sort-midi-events midi-events)))
      
    
(defmethod phrase-duration ((ghost ghost))
  (phrase-duration (property ghost :source)))

(defgeneric ghost-p (object))
(defmethod ghost-p ((object t)) nil)
(defmethod ghost-p ((ghost ghost)) t)
