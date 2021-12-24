;;;; CYCO parts mixer
;;;;
;;;; Defines MIXER class.
;;;;
;;;; MIXER is a part for generating static MIDI volume (controller 7) events.
;;;; See CONTROLLERS part for more general controller events.
;;;;
;;;; In general MIXER is used to set fixed amplitudes between instruments.
;;;; CONTROLLERS parts may then create dynamic volume changes using 'expression'
;;;; controller events (controller number 11).
;;;;

(in-package :cyco-part)

(constant +mixer-properties+
	  (append +part-properties+ '()))

(defclass mixer (part)
  ((events
    :initform '()
    :accessor mixer-events)))
   
(defgeneric mixer-p (item))
(defmethod mixer-p ((item t)) nil)
(defmethod mixer-p ((item mixer)) t)

(labels ((process-event-time
	  (mixer event)
	  (let* ((cuefn (property mixer :cue-function))
		 (t-spec (car event)))
	    (+ (property mixer :shift)
	       (funcall cuefn mixer t-spec))))

	 (process-channel
	  (mixer sub-event)
	  ;; ISSUE: Not sure why FIND-CHILD and +ROOT-INSTRUMENT+ are not
	  ;; being exported by the CYCO pacakge !
	  (let* ((c-spec (car sub-event))
		 (chan (cond ((and (integerp c-spec)(plusp c-spec)(<= c-spec 16))
			      c-spec)
			     ((symbolp c-spec)
			      (cyco::find-child cyco::+root-instrument+ c-spec))
			     (t nil))))
	    (if chan
		(channel chan)
	      (cyco-error (sformat "MIXER ~A, invalid sub-event ~A" (name mixer) sub-event)))))

	 (process-value
	  (mixer sub-event)
	  (let* ((v-spec (or (cdr sub-event) 127)))
	    (or (and (integerp v-spec)
		     (max 0 (min 127 v-spec)))
		(cyco-error (sformat "MIXER ~A, invalid sub-event ~A" (name mixer) sub-event)))))

	 (process-event
	  (mixer event)
	  (let ((time (process-event-time mixer event))
		(acc '()))
	    (dolist (sub-event (cdr event))
	      (let ((channel (process-channel mixer sub-event))
		    (value (process-value mixer sub-event)))
		(push (cons time (midi-control-change (1- channel) 7 value)) acc)))
	    (reverse acc))) )

	(defun make-mixer (name &key
				section
				cuefn
				shift
				tempo unit bars beats subbeats
				render-once
				remarks
				events)
	  (let* ((part-name (->cyco-symbol name))
		 (parent (validate-section part-name section))
		 (mixer (make-instance 'mixer
				       :properties +mixer-properties+
				       :name part-name
				       :remarks (->string (or remarks ""))
				       :transient t))
		 (midi-events '()))
	    (connect parent mixer)
	    (put mixer :cue-function cuefn)
	    (put mixer :shift (scale-time-parameter (or shift 0) mixer))
	    (put mixer :tempo tempo)
	    (put mixer :unit unit)
	    (put mixer :bars bars)
	    (put mixer :beats beats)
	    (put mixer :subbeats subbeats)
	    (init-time-signature mixer)
	    (put mixer :render-once render-once)
	    (put mixer :transposable nil)
	    (put mixer :reversible nil)
	    (dolist (event (->list events))
	      (dolist (midi-event (process-event mixer event))
		(push midi-event midi-events)))
	    (setf (mixer-events mixer)(reverse midi-events))
	    mixer)))


(defmacro mixer (name  &key
		       section
		       cuefn
		       shift
		       tempo unit bars beats subbeats
		       render-once
		       remarks
		       events)
  `(progn
     (part-banner (name ,section) ',name)
     (let ((mix (make-mixer ',name
			      :section ,section
			      :cuefn ,cuefn
			      :shift ,shift
			      :tempo  ,tempo 
			      :unit  ,unit 
			      :bars  ,bars 
			      :beats  ,beats 
			      :subbeats ,subbeats
			      :render-once ,render-once
			      :remarks ,remarks
			      :events ,events)))
       (defparameter ,name mix)
       mix)))

(defmethod reset ((mixer mixer)) mixer)

(defmethod clone ((mother mixer) &key new-name new-parent)
  (let* ((frmt (or new-name "~A"))
	 (name (->symbol (sformat frmt (name mother))))
	 (parent (or new-parent (parent mother)))
	 (daughter (make-mixer name
			       :section parent
			       :remarks (remarks mother))))
    (copy-part-properties mother daughter)
    (copy-time-signature mother daughter)
    (setf (mixer-events daughter)
	  (clone (mixer-events mother)))
    daughter))

(defmethod render-once ((mixer mixer) &key (offset 0.0) &allow-other-keys)
  (if (muted-p mixer)(return-from render-once '()))
  (let* ((midi-events ())
	 (time-shift (+ (property mixer :shift) offset)))
    (dolist (event (mixer-events mixer))
      (let ((time (+ time-shift (car event)))
	    (message (cdr event)))
	(push (cons time message) midi-events)))
    (sort-midi-events midi-events)))
  

(constant +mixer-docstring+
      "MAKE-MIXER and MIXER are identical except the later binds the new
part to the symbol name while the former does not.  Quote the name
argument to MAKE-MIXER and leave it unquoted for MIXER.

Mixer events have the following form:

     (time (channel-a . value-a)(channel-b . value-b) ... (channel-n . value-b))

Where channel may be an integer MIDI channel (1..16 inclusive) or the name
of an instrument.   And value is an integer between 0 and 127 inclusive.

The time clause must be in a format expected by the cue-function.  By
default time is specified as a list (bar beat sub-beat).")

(setf (documentation 'make-mixer 'function) +mixer-docstring+)
(setf (documentation 'mixer 'function) +mixer-docstring+)

