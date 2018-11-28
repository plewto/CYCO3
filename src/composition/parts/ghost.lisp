;;;; CYCO Ghost
;;;;

(def-type-predicate ghost-p)

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
The ghosted notes may be rechannelized, delayed, transposed, inverted,
velocity scaled and key-mapped.

A potential source of confusion is the time-signature of the Ghost.  The
ghost may only be a child of a section, and thus inherits the
time-signature of the section.  Delay times are specified in terms of the
section time-signature.

The tracked part may have a different time-signature, specifically it may
have a lower bar count.  When the ghost part is rendered it uses the
tracked part's duration, and -not- the section duration.  As a consequence
the Ghost phrase-duration method returns the duration of the tracked part
and -not- the normally expected value of the nominal time-signature.

A Ghost may not have child nodes."))


(defmethod ghost-p ((obj ghost)) t)

(defun make-ghost (name source track &key
			(delay nil)
			(transpose nil)
			(pivot nil)
			(ampscale 1.0)
			(keytable +default-keytable+)
			(outchan nil)
			(remarks ""))
  "Create new GHOST part.
name       - Symbol
source     - Part to be tracked.
track      - Instrument or MIDI channel to track.
             A part may contain multible channels, a Ghost however only
	     tracks a single channel.
:delay     - Ghosted events delay time.  The delay is specified in terms
             of the parent Section's time-signature and cue-function.
:transpose - Transpositino of ghosrted notes.
:pivot     - Pivot for key inversion
:keytable  - A vector of length 128 which maps keynumbers.  A value of -1 
             produces a rest.  Defaults to +DEFAULT-KEYTABLE+
:ampscale  - Amplitude scaling factor, default 1.0
:outchan   - Ghosted note MIDI channel, defaults to the track channel.
:remakrs   - 

Key number manipulation function is the ccomposition:

    keytable[invert(transpose(key))]"

  (if (not (part-p source))
      (progn
	(cyco-type-error 'make-ghost 'part source
			 (sformat "Source argument to MAKE-GHOST must be a PART."))
	(return-from make-ghost)))
  (let* ((parent (parent source))
	 (ghst (make-instance 'ghost
			     :properties +ghost-properties+
			     :name name
			     :remarks (->string (or remarks ""))
			     :transient t))
	 (cuefn (property parent :cue-function)))
    (connect parent ghst)
    (copy-time-signature parent ghst)
    (put ghst :source source)
    (put ghst :delay (funcall cuefn parent delay))
    (put ghst :transpose-steps transpose)
    (put ghst :pivot pivot)
    (put ghst :ampscale (or ampscale 1.0))
    (put ghst :keytable (or keytable +default-keytable+))
    (put ghst :track-channel (channel track))
    (put ghst :output-channel (or (and outchan (channel outchan :resolve))
				  (channel track)))
    (put ghst :transposable nil)
    (put ghst :reversible nil)
    ghst))

(defmacro ghost  (name source track &key
			(delay nil)
			(transpose nil)
			(pivot nil)
			(ampscale 1.0)
			(keytable +default-keytable+)
			(outchan nil)
			(remarks ""))
  "The GHOST macro is identical to the MAKE-GHOST function except it binds
the new part to a symbol of the same name."
  `(progn
    (part-banner (name (parent ,source)) ',name)
    (let ((ghst (make-ghost ',name ,source ,track
			    :delay ,delay
			    :transpose ,transpose
			    :pivot ,pivot
			    :ampscale ,ampscale
			    :keytable ,keytable
			    :outchan ,outchan
			    :remarks ,remarks)))
      (defparameter ,name ghst)
      ghst)))

(defmethod connect ((parent ghost)(child t))
  "Ghost objects are not allowed to have child nodes."
  (cyco-type-error 'connect 'cyco-node parent
		   (sformat "GHOST objects may not have child nodes.")))

(defmethod reset ((ghst ghost))
  "Calls reset on the tracked part."
  (reset (property ghst :source)))

(defmethod clone ((src ghost) &key new-name new-parent)
  (let* ((frmt (or new-name "~A"))
	 (name (->symbol (sformat frmt (name src))))
	 (parent (or new-parent (parent src)))
	 (source (clone (property src :source) :new-name frmt :new-parent parent))
	 (ghst (make-ghost name source (property src :track-channel)
			   :delay (property src :delay)
			   :transpose (property src :transpose-steps)
			   :pivot (property src :pivot)
			   :ampscale (property src :ampscale)
			   :keytable (property src :keytable)
			   :outchan (property src :output-channel)
			   :remarks (property src :remarks))))
    (copy-time-signature src ghst)
    ghst))

(labels ((allow (msg ci)
		(and (or (midi-note-on-p msg)
			 (midi-note-off-p msg))
		     (= (channel-index msg) ci)))
	 
	 (key-wrangle (kn ktab xpose pivot)
		      (let ((kn2 (invert (transpose kn xpose) pivot)))
			(if (and (<= 0 kn2)(< kn2 128))
			    (aref ktab kn2)
			  -1)))
	 
	 (amp-wrangle (velocity ampscale)
		      (let ((v2 (truncate (* ampscale velocity))))
			(limit v2 0 127))) )

  (defmethod render-once ((ghst ghost) &key (offset 0.0))
    (let ((acc '())
	  (delay (property ghst :delay))
	  (xpose (property ghst :transpose-steps))
	  (pivot (property ghst :pivot))
	  (ktab (property ghst :keytable))
	  (ampscale (property ghst :ampscale))
	  (ci-track (1- (property ghst :track-channel)))
	  (ci-out (1- (property ghst :output-channel))))
      (dolist (evn (render-once (property ghst :source) :offset offset))
	(let ((time (car evn))
	      (msg (cdr evn)))
	  (if (allow msg ci-track)
	      (let ((kn (key-wrangle (data msg 0) ktab xpose pivot))
		    (velocity (amp-wrangle (data msg 1) ampscale)))
		(if (not (rest-p kn))
		    (push (cons (+ time delay)
				(if (midi-note-on-p msg)
				    (midi-note-on ci-out kn velocity)
				  (midi-note-off ci-out kn velocity)))
			  acc))))))
      (sort-midi-events acc))) )

(defmethod phrase-duration ((ghst ghost))
  (phrase-duration (property ghst :source)))

(defmethod render-n ((ghst ghost)(n integer) &key (offset 0.0))
  (reset ghst)
  (let ((template (render-once ghst))
	(delay (property ghst :delay))
	(shift (or (property ghst :shift) 0))
	(acc '())
	(period (phrase-duration (property ghst :source))))
    (dotimes (i (if  (property ghst :render-once) 1 n))
      (let ((tshift (+ delay shift (* i period) offset)))
	(dolist (evn template)
	  (let ((time (+ tshift (car evn)))
		(msg (clone (cdr evn))))
	    (push (cons time msg) acc)))))
    (sort-midi-events acc)))
      
    
