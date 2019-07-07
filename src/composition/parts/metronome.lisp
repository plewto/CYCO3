;;;; CYCO
;;;;
;;;; The metronome is a specialized part which generates 3 events types:
;;;;   'phrase - First beat of phrase 
;;;;   'bar    - First beat of bar, except for the first bar
;;;;   'beat   - all other beats.
;;;;

(let ((docstring
   "Creates METRONOME part.  

Default values are generally acceptable for a generic metronome pattern.
:name    - Symbol, defaults to <SECTION-NAME>-METRONOME
:section - Parent Section, defaults to current section of *project*
:cuefn   - Cuing function, default inherits from section.
:tempo   - tempo, default inherits from section.
:unit    - unit, default inherits from section.
:bars    - bars, default inherits from section.
:beats   - beats, default inherits from section.
:cue     - cue pattern, default produces an event on all beats.
:key     - key number pattern.  
           The *metronome* instrument uses special keynumber mapping and
           which recognizes 'phrase 'bar and 'beat keys.  phrase is the first
           beat of the first bar.  bar appears on the first beat of every
           bar, except for the first bar.  beat appears on all other beats.
:amp     - dynamic pattern.  
           The recognized dynamic symbols are identical  to the keynumber symbols.
:instrument - Defaults to the global *metronome* instrument.  The instrument
              should use the specialized metronome-keynumber-map and
              metronome-dynamic-map.   Initially *metronome* is set 
              to a null instrument and should be set by the CYCO
              orchestra configuration."))     

  (flet ((validate-section (section)
			   (cond ((section-p section)
				  section)
				 ((and (project-p *project*)
				       (property *project* :current-section))
				  (property *project* :current-section))
				 (t (cyco-composition-error 'make-metronome
							    "No default Project"))))

	 (make-cue-list (bars beats)
			(let ((cue-list '()))
			  (dotimes (bar bars)
			    (dotimes (beat beats)
			      (push (list (1+ bar)(1+ beat) 1) cue-list)))
			  (reverse cue-list)))

	 (make-key-pattern (bars beats)
			   (let ((key-list '()))
			     (dotimes (bar bars)
			       (dotimes (beat beats)
				 (push (cond ((and (zerop bar)(zerop beat)) 'phrase)
					     ((zerop beat) 'bar)
					     (t 'beat))
				       key-list)))
			     (reverse key-list))))
  
    (defun make-metronome (&key name
				section
				cuefn
				tempo unit bars beats
				cue key amp
				(instrument *metronome*))
      docstring
      (setf section (validate-section section))
      (setf name (->symbol (or name (sformat "~A-~A" (name section) "METRONOME"))))
      (setf tempo (or tempo (property section :tempo)))
      (setf unit (or unit (property section :unit)))
      (setf bars (or bars (property section :bars)))
      (setf beats (or beats (property section :beats)))
      (let* ((cue-list (or cue (make-cue-list bars beats)))
	     (key-pattern (or key (make-key-pattern bars beats)))
	     (amp-pattern (or amp (clone key-pattern)))
	     (metronome-part (make-qball name instrument
			     :section section
			     :cuefn cuefn
			     :tempo tempo
			     :unit unit
			     :bars bars
			     :beats beats
			     :cue cue-list
			     :key key-pattern
			     :amp amp-pattern
			     :transposable nil
			     :reset-on-repeat t)))
	metronome-part))))
  

(defmacro metronome (name &key
			  section
			  tempo unit bars beats
			  cue key amp
			  (instrument *metronome*))
  "Same as make-metronome except the metronome part is bound to the 
symbol name."
  `(let* ((met (make-metronome
		:name 'metronome
		:section ,section
		:tempo ,tempo
		:unit ,unit
		:bars ,bars
		:beats ,beats
		:cue ,cue
		:key ,key
		:amp ,amp
		:instrument ,instrument)))
     (defparameter ,name met)
     met))



