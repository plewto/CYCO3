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
		    :key-pattern
		    :articulation-pattern
		    :dynamic-pattern
		    :reset-on-repeat)))

(defclass qball (part) nil)

(labels ((validate-section
	  (part-name section)
	  (let ((s (cond ((section-p section)
			  section)
			 ((and section (not (section-p section)))
			  (cyco-type-error 'make-qball '(section nil) section)
			  nil)
			 ((not (project-p *project*))
			  (cyco-composition-error
			   'make-qball
			   (sformat "No default project while creating qball ~A" part-name))
			  nil)
			 (t (property *project* :current-section)))))
	    s))

	 ;; Checks that instruments argument is valid.
	 ;;   1) A single instrument --> converted to list --> 2
	 ;;   2) A list of instruments --> converted to INSTRUMENT-LAYER pattern.
	 ;;   3) A Pattern of instruments.
	 ;; Returns Pattern.
	 (validate-instruments
	  (part-name instruments)
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
  			  (cuefn #'bar)
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
    "Creates new QBALL instance.
Most keyname arguments default to the parent node values.
name
instruments - Pattern of instruments. A single instrument or list
              of instruments is converted to a Cycle pattern.
:section  - Parent section, defaults to current-section of *project*
:cuefn    - Cueing function
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
used."
    (let ((parent (or (validate-section name section)
    		      (return-from make-qball nil))))
      (let* ((instrument-pattern (or (validate-instruments name instruments)
    				     (return-from make-qball nil)))
    	     (qball (make-instance 'qball
    				   :properties +qball-properties+
    				   :name name
    				   :remarks (->string (or remarks ""))
    				   :transient t)))
    	(put qball :instruments instrument-pattern)
    	(put qball :tempo tempo)
    	(put qball :unit unit)
    	(put qball :bars bars)
    	(put qball :beats beats)
    	(put qball :subbeats subbeats)
    	(put qball :cue-function cuefn)
	(put qball :render-once render-once)
    	(put qball :transposable transposable)
	(put qball :reversible reversible)
    	(put qball :muted nil)
    	(put qball :cue-cycle (->cycle cue))
    	(put qball :key-pattern (->pattern (or key '(60))))
    	(put qball :articulation-pattern (->pattern (or dur 1.0)))
    	(put qball :dynamic-pattern (->pattern (or amp 0.5)))
	(reset qball)
    	(put qball :reset-on-repeat reset-on-repeat)
    	(connect parent qball)
    	(put qball :shift (if shift (float shift) 0.0))
    	(set-cyco-prompt)
    	qball))))

(defmacro qball (name instruments &key
  		      section
  		      (cuefn #'bar)
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
     (let ((qb (make-qball ',name ,instruments
			   :section ,section
			   :cuefn ,cuefn
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
       (defparameter ,name qb)
       qb)))
						
(defmethod transpose ((qb qball)(n t))
  (if (property qb :transposable)
      (put qb :key-pattern
	    (transpose (property qb :key-pattern) n)))
  qb)

(defmethod invert ((qb qball)(pivot t))
  (if (property qb :transposable)
      (put qb :key-pattern
	    (invert (property qb :key-pattern)
		    (keynumber pivot))))
  qb)

(defmethod retrograde ((qb qball))
  (if (property qb :reversible)
	(retrograde (property qb :key-pattern)))
  qb)

(defmethod reset ((qb qball))
  (reset (property qb :cue-cycle))
  (reset (property qb :key-pattern))
  (reset (property qb :articulation-pattern))
  (reset (property qb :dynamic-pattern))
  qb)

(defmethod soft-reset ((qb qball))
  (reset (property qb :cue-cycle)))

(labels ((render-event
	  (qball time ilist keylist art dyn)
	  (let ((bcc '())
		(ascale (if (numberp art)
			    1.0
			  (beat-duration qball))))
	    (dolist (inst ilist)
	      (let* ((ci (channel-index inst))
		     (kmap (property inst :keynumber-map))
		     (dmap (property inst :articulation-map))
		     (amap (property inst :dynamic-map))
		     (dur (* (funcall dmap art) ascale))
		     (end-time (+ time dur))
		     (amp (funcall amap dyn)))
		(dolist (k keylist)
		  (let ((keynumber (funcall kmap k)))
		    (if (not (or (rest-p keynumber)
				 (rest-p dur)
				 (rest-p amp)))
			(let ((vel (norm->midi-data amp))) 
			  (push (cons time (midi-note-on ci keynumber vel)) bcc)
			  (push (cons end-time (midi-note-off ci keynumber 0)) bcc)))))))
	    bcc)))

  (defmethod render-once ((qb qball) &key (offset 0.0))
    (if (not (muted-p qb))
	(progn
	  (if (property qb :reset-on-repeat)
	      (reset qb)
	    (soft-reset qb))
	  (let ((acc '())
		(cuefn (property qb :cue-function)))
	    (dolist (tspec (next (property qb :cue-cycle) :all))
	      (let ((time (+ offset (funcall cuefn qb tspec)))
		    (keylist (->list (next (property qb :key-pattern))))
		    (art (next (property qb :articulation-pattern)))
		    (dyn (next (property qb :dynamic-pattern)))
		    (ilist (->list (next (property qb :instruments)))) )
		(setf acc (append acc (render-event qb time ilist keylist art dyn)))))
	    (dolist (c (reverse (children qb)))
	      (setf acc (append acc (render-once c :offset offset))))
	    (sort-midi-events acc))))))

(defmethod render-n ((qb qball)(n integer) &key (offset 0.0))
  (reset qb)
  (let ((period (phrase-duration qb))
	(acc '()))
    (dotimes (i (if (property qb :render-once) 1 n))
      (dolist (evn (render-once qb))
	(let ((reltime (car evn))
	      (msg (cdr evn)))
	  (push (cons (+ offset (* i period) reltime) (clone msg)) acc))))
    (sort-midi-events acc)))


(defmethod clone ((src qball) &key new-name new-parent)
  (let* ((frmt (or new-name "~A"))
	 (name (->symbol (sformat frmt (name src))))
	 (parent (or new-parent (parent src)))
	 (prt (make-qball name (clone (property src :instruments))
			  :section parent
			  :cuefn (property src :cue-function)
			  :transposable (property src :transposable)
			  :cue (clone (property src :cue-cycle))
			  :key (clone (property src :key-pattern))
			  :dur (clone (property src :articulation-pattern))
			  :amp (clone (property src :dynamic-pattern))
			  :reset-on-repeat (property src :reset-on-repeat)
			  :remarks (remarks src)
			  )))
    (copy-time-signature src prt)
    (put prt :shift (property src :shift))
    (dolist (c (children src))
      (clone c :new-name frmt :new-parent prt))
    prt))



