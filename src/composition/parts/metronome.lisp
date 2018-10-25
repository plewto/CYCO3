;;;; PigIron CYCO composition parts metronome
;;;;


(defun make-metronome (&key name
			    section
			    cuefn
			    tempo unit bars beats
			    cue key amp
			    (instrument *metronome*))
  "Creates METRONOME part.   The default values are generally acceptable
for a generic metronome pattern.
:name    - Symbol, defaults to <SECTION-NAME>-METRONOME
:section - Parent Section, defaults to current section of *project*
:cuefn   - Cuing function, default inherits from section.
:tempo   - tempo, default inherits from section.
:unit    - unit, default inherits from section.
:bars    - bars, default inherits from section.
:beats   - beats, default inherits from section.
:cue     - cue pattern, default produces an event on all beats.
:key     - key number pattern.  The *metronome* instrument uses
           special keynumber mapping and recognized 'phrase 'bar
           and 'beat keys.  phrase is the first beat of the first bar.
           bar appears on the first beat of every bar, except for the 
           first bar.  beat appears on all other beats.
:amp     - dynamic pattern.  The recognized dynamic symbols are identical 
           to the keynumber symbols.
:instrument - Defaults to the global *metronome* instrument.  The instrument
              should use the specialized metronome-keynumber-map and
              metronome-dynamic-map.   Initially *metronome* is set 
              to a null instrument and should be set by the CYCO
              orchestra configuration."
  (setf section (cond ((section-p section)
		       section)
		      ((and (project-p *project*)
			    (property *project* :current-section))
		       (property *project* :current-section))
		      (t (cyco-no-project-error 'make-metronome))))
  (setf name (->symbol (or name
			   (sformat "~A-~A" (name section) "METRONOME"))))
  (setf tempo (or tempo (property section :tempo)))
  (setf unit (or unit (property section :unit)))
  (setf bars (or bars (property section :bars)))
  (setf beats (or beats (property section :beats)))
  (let* ((cue-list (or cue
		       (let ((acc '()))
			 (dotimes (br bars)
			   (dotimes (bt beats)
			     (push (list (1+ br) (1+ bt) 1) acc)))
			 (reverse acc))))
	 (key-pattern (cycle :of (or key
				     (let ((acc '()))
				       (dotimes (br bars)
					 (dotimes (bt beats)
					   (push (cond ((and (zerop br)(zerop bt)) 'phrase)
						       ((zerop bt) 'bar)
						       (t 'beat))
						 acc)))
				       (reverse acc)))))
	 (amp-pattern (or amp (clone key-pattern)))
	 (qb (make-qball name instrument
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
    qb)) 
			 

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
