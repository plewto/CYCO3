;;;; CYCO
;;;;
;;;; A QBall is a recombinative note generator.
;;;; The user specifies instrument, time, key-number, duration and
;;;; dynamic patterns.  These are recombined according to their
;;;; individual pattern types to produce MIDI note events.
;;;;
;;;; ISSUE: Check that 'REST' articulations are properly ignored.
;;;;        They may be producing note-on events but not corresponding
;;;;        note off events.

(constant +qball-properties+
	  (append +part-properties+
		  '(:shift
		    :render-once
		    :cue-cycle
		    :shuffle-function
		    :key-pattern
		    :articulation-pattern
		    :dynamic-pattern
		    :reset-on-repeat)))

(defclass qball (part) nil)


(let ((docstring 
   "Creates new QBALL instance.
Most keyname arguments default to the parent node values.
name
instruments - Pattern of instruments. A single instrument or list
              of instruments is converted to a Cycle pattern.
:section  - Parent section, defaults to current-section of *project*
:cuefn    - Cueing function
:shuffle  - Shuffle function
:shift    - Fixed offset in seconds added to initial time.
:tempo    - Tempo, beats per minute, defaults to parent tempo
:unit     - Time signature beat unit
:bars     - Number of bars
:beats    - Number of beats per bar
:subbeats - Number of sub-beats per beat
:render-once  -
:transposable - Boolean, 
:cue  - List of times in a format required by cuefn.
        For the default #'bar cuefn, times are specified as a list
        (bar beat subbeat).  Assuming 4/4 time, the first four beats
        of the first bar is specified by ((1 1 1)(1 2 1)(1 3 1)(1 4 1)).
:shuffle - ISSUE add documentation
:key  - Pattern of keynumbers. A single key or list of keys is converted
        to a Cycle pattern.  Nested list are treated as chords.
        A C major arpeggio, followed by a G major chord is specified by 
        (C3 E3 G3 (G3 B3 D4)).   Patterns other then Cycles are allowed.
:dur  - Note on duration. A single value or list of values are converted to 
        a Cycle.  Numeric values are treated as absolute.  Metric expressions
        are scaled by the current tempo.
:amp  - Note amplitudes, A single value or list of values are converted to 
        a Cycle. 
:reset-on-repeat - Boolean.  If true all patterns are reset on each 
        repetition and the qball will produce the same results on
        each call to render-once.  If false the patterns are not reset
        and each call to render-once may produce a different set of events,
        depending on the relative lengths of the patterns.
:remarks - Optional text.

Each :key :dur and :amp values are processed by the keynumber-map,
articulation-map and dynamic-map respectively of each instrument as they are
used."))
  
  (labels ((validate-section (part-name section)
			     (cond ((section-p section)
				    section)
				   ((and section (not (section-p section)))
				    (cyco-type-error 'make-qball '(section nil) section)
				    nil)
				   ((not (project-p *project*))
				    (cyco-composition-error
				     'make-qball
				     (sformat "No default project while creating qball ~A" part-name))
				    nil)
				   (t (property *project* :current-section))))
	   
	   ;; Checks that instruments argument is valid.
	   ;;   1) A single instrument      --> converted to list --> 2
	   ;;   2) A list of instruments    --> converted to INSTRUMENT-LAYER pattern.
	   ;;   3) A Pattern of instruments --> Use as is.
	   ;; Returns Pattern.
	   (validate-instruments (part-name instruments)
				 (cond
				  ((pattern-p instruments)
				   instruments) ;; does not check pattern elements
				  ((listp instruments)
				   (or (and (every #'instrument-p instruments)
					    (instrument-layer :of instruments))
				       (cyco-type-error 'make-qball "List of instruments"
							instruments
							(sformat "part name is ~A" part-name))))
				  ((instrument-p instruments)
				   (instrument-layer :of (->list instruments)))
				  (t (cyco-type-error 'make-qball
						      "Instrument or list of instruments"
						      instruments
						      (sformat "part name is ~A" part-name))))) )
    
    (defun make-qball (name instruments &key
			    section
			    cuefn
			    shuffle 
			    shift
			    tempo unit bars beats subbeats
			    render-once
			    (transposable nil)
			    (reversible nil)
			    cue
			    (key '(60))
			    (dur '(q))
			    (amp '(mf))
			    (reset-on-repeat nil)
			    remarks)
      docstring
      (let* ((parent (or (validate-section name section)
			 (return-from make-qball nil)))
	     (instrument-pattern (or (validate-instruments name instruments)
				     (return-from make-qball nil)))
	     (new-qball (make-instance 'qball
				   :properties +qball-properties+
				   :name name
				   :remarks (->string (or remarks ""))
				   :transient t)))
	  (put new-qball :instruments instrument-pattern)
	  (put new-qball :tempo tempo)
	  (put new-qball :unit unit)
	  (put new-qball :bars bars)
	  (put new-qball :beats beats)
	  (put new-qball :subbeats subbeats)
	  (put new-qball :cue-function cuefn)
	  (put new-qball :shuffle-function shuffle)
	  (put new-qball :render-once render-once)
	  (put new-qball :transposable transposable)
	  (put new-qball :reversible reversible)
	  (put new-qball :muted nil)
	  (put new-qball :cue-cycle (->cycle cue))
	  (put new-qball :key-pattern (->pattern (or key '(60))))
	  (put new-qball :articulation-pattern (->pattern (or dur 1.0)))
	  (put new-qball :dynamic-pattern (->pattern (or amp 0.5)))
	  (reset new-qball)
	  (put new-qball :reset-on-repeat reset-on-repeat)
	  (connect parent new-qball)
	  (put new-qball :shift (if shift (float shift) 0.0))
	  (set-cyco-prompt)
	  new-qball)))) 
  
(defmacro qball (name instruments &key
		      section
		      cuefn
		      shuffle
		      shift
		      tempo unit bars beats subbeats
		      render-once
		      transposable
		      reversible
		      cue
  		      key 
  		      dur
  		      amp
  		      reset-on-repeat
  		      remarks)
  "Same as make-qball except the new qball object is bound to symbol 
named name."
  `(progn
     (part-banner (name ,section) ',name)
     (let ((new-qball (make-qball ',name ,instruments
			   :section ,section
			   :cuefn ,cuefn
			   :shuffle ,shuffle
			   :shift ,shift
			   :render-once ,render-once
			   :transposable ,transposable
			   :reversible ,reversible
			   :tempo ,tempo
			   :unit ,unit
			   :bars ,bars
			   :beats ,beats
			   :subbeats ,subbeats
			   :cue ,cue
			   :key ,key
			   :dur ,dur
			   :amp ,amp
			   :reset-on-repeat ,reset-on-repeat
			   :remarks ,remarks)))
       (defparameter ,name new-qball)
       new-qball)))
						
(defmethod transpose ((qball qball)(n t))
  (if (property qball :transposable)
      (put qball :key-pattern
	    (transpose (property qball :key-pattern) n)))
  qball)

(defmethod invert ((qball qball)(pivot t))
  (if (and pivot (property qball :transposable))
      (put qball :key-pattern
	    (invert (property qball :key-pattern)
		    (keynumber pivot))))
  qball)

(defmethod retrograde ((qball qball))
  (if (property qball :reversible)
	(retrograde (property qball :key-pattern)))
  qball)

(defmethod reset ((qball qball))
  (reset (property qball :cue-cycle))
  (reset (property qball :key-pattern))
  (reset (property qball :articulation-pattern))
  (reset (property qball :dynamic-pattern))
  qball)

(defmethod soft-reset ((qball qball))
  (reset (property qball :cue-cycle)))

(labels ((render-event (qball time instrument-list key-list articulation dynamic)
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


(defmethod clone ((source qball) &key new-name new-parent)
  (let* ((frmt (or new-name "~A"))
	 (name (->symbol (sformat frmt (name source))))
	 (parent (or new-parent (parent source)))
	 (new-qball (make-qball name (clone (property source :instruments))
			  :section parent
			  :cuefn (property source :cue-function)
			  :shuffle (property source :shuffle-function)
			  :shift (property source :shift)
			  :transposable (property source :transposable)
			  :reversible (property source :reversible)
			  :cue (clone (property source :cue-cycle))
			  :key (clone (property source :key-pattern))
			  :dur (clone (property source :articulation-pattern))
			  :amp (clone (property source :dynamic-pattern))
			  :reset-on-repeat (property source :reset-on-repeat)
			  :remarks (remarks source)
			  )))
    (copy-time-signature source new-qball)
    (put new-qball :shift (property source :shift))
    (dolist (sub-part (children source))
      (clone sub-part :new-name frmt :new-parent new-qball))
    new-qball))
