# CYCO QBall Pattern

A *QBALL* is a type of recombinant PART which creates events by combining
instrument, keynumber, articulation and dynamic patterns.  It is
particularly suited for percussion parts.  A typical usage might look something
like this.

    (qball foo snare
	    :cue '((1 1 1 )(1 2 1)(1 3 3)(1 4 3))
	    :key '(x x rim)
	    :amp (cycle :of (list 'ff (dice :of '(f mf mp)))))
		
This examples uses a snare instrument whose keynumber-map recognizes 'X and
'RIM as key-numbers.  The :CUE list specifies 4-events and the
three-element key list is converted to a CYCLE pattern.  The :amp pattern
alternates between an FF dynamic and one randomly selected by the DICE
pattern.

**(MAKE-QBALL name instruments &key ...)** <br>
**(QBALL name instruments &key ...)**

    Creates new QBALL part in the current section.
	
	QBALL and MAKE-QBALL are identical except the former binds the new
    part to the symbol name while the later does not.  name should be
    quoted for MAKE-QBALL and unquoted for QBALL.
		

	The instruments argument may be a single INSTRUMENT, list of
	instruments or a pattern of instruments.

	Keyword arguments:
	
	:section      - parent section, defaults to current-section of *project*
    :cuefn        - cue-function, inherited from section.
	:shuffle      - shuffle function, inherited from section.
    :Shift        - float, offset in seconds added to all events, default 0.0
    :tempo        - time-signature tempo, inherited from section 
    :unit         - time-signature beat-unit, inherited from section 
    :bars         - time-signature bar-count, inherited from section 
    :beats        - time-signature beats per bar, inherited from section
    :subbeats     - time-signature subbeats per beat, inherited from section
    :render-once  - Boolean, if true the qball events are produced once only 
                    and are not repeated.
    :transposable - Boolean, if true this part is subject to transpose and
                    key invert transformations.
    :reversible   - Boolean, if true this part is subject to retrograde
                    transformations.
    :cue          - See below
    :key          - See below
    :dur          - See below
    :amp          - See below
    :reset-on-repeat - See below
    :remarks      - Optional remarks text.
	
### Instrument Pattern

The instrument argument may take one of three forms:

1. A single instrument
2. List of instruments to be layered together, all instruments play all
   events. 
3. Pattern of instruments.  The pattern type determines when each instrument
   is used. 
   
        (qball foo organ ...)

            Organ plays all notes
		 
        (qball foo (list organ piano) ...)
	  
	        Organ and piano are layered.
			
		(qball foo (cycle :of (list organ piano)) ...)
		
		    Organ and piano play alternate notes.
			
### CUE list

The list specified by :CUE sets the number and timing of events.   An event
is produced for each element in the cue-list.   By default the cue-list is
interpreted by the BAR function.  

    :cue '((1 1 1)(1 2 1)(1 3 1)(1 4 1)) ;; produces an event on each beat of bar 1.
	

### Key Pattern

The :KEY argument sets the keynumber pattern and is converted to a CYCLE by
default.   A single key value is allowed.

    :key 'c4         --> :key (cycle :of 'c4)  all events play c4.
	:key '(c4 e4 g4) --> :key (cycle :of '(c4 e4 g4))
	
	Embedded list are treated as chords.
	
	:key '(c4 e4 g4 (c4 e4 a4)) --> play c-major arpeggio followed by a-minor chord

	
Prior to use each element of the :KEY list is processed by the instrument's
keynumber-map function.  Unrecognized keys are treated as rest.  The
following example layers snare and tom instruments.  Both instruments
recognize 'X and 'BOUNCE as keynumbers, only the snare recognizes 'RIM

    (instrument snare :channel 10
	    :keynumber-map (symbolic-keynumber-map '((x 30)
		                                         (rim 32)
												 (bounce 34))))
												 
	(instrument tom :channel 10
	    :keynumber-map (symbolic-keynumber-map '((x 60)
												 (bounce 64))))
												 
	(qball percussion-part (list snare tom)
	   :cue '((1 1 1)(1 2 1)(1 3 1))
	   :key '(x rim bounce))
	   
The snare plays all three events, the tom plays 2, ignoring RIM.


### Articulation (duration) Pattern


The articulation pattern defaults to a cycle.

    :dur '(q h w) --> :dur (cycle :of '(q h w))
	
Each element of the articulation list is either a METRIC-EXPRESSION or a
numeric value.   Metric expressions are scaled by the current tempo while
numeric values are absolute times in seconds.

    :dur '(q. 2)
	 
	    Pattern of alternating dotted quarter note and 2 second note
		duration.
		
The duration values are processed by each instrument's articulation-map.
Unrecognized values should produce a rest.


### Dynamic Pattern

The dynamic pattern defaults to a cycle.

    :amp '(p pp ppp) --> :key (cycle :of '(p pp ppp))
	
	
### Parameter Combinations

The :CUE pattern sets the number and time for each event.   The instrument,
key, duration and amplitude patterns produce appropriate parameters for
each event.  


    (qball foo piano
	    :cue '((1 1 1)(1 2 1)(1 3 1)(1 4 1))
		:key '(c4 e4 g4)
		:amp (cycle :of (list 'FF (dice :of '(P F))))
		:dur 'q)
		
	The above qball produces four events; one each for beats 1,2,3 and 4 of
	bar 1. 
	
	The three key values cycle over these four events to produce c e g c
	
	The amp list alternately produces an FF and a randomly selected P or F
	dynamic. 
	
	A quarter-note duration is used for all events.
	
	(1 1 1)  C4  amp FF
	(1 2 1)  E4  amp P or F
	(1 3 1)  G4  amp FF
	(1 4 1)  c4  amp P or F

### RESET ON REPEAT

When the bar-count of a QBALL is less then that of the parent section, the
QBALL is repeated as needed to fill the section's duration (assuming
RENDER-ONCE is nil).   The :RESET-ON-REPEAT flag indicates if the internal
patterns are reset prior to each repetition. <br>

If RESET-ON-REPEAT is true, then each repetition will produce the same
events (ignoring any random pattern types like DICE or BAG).  If
RESET-ON-REPEAT is nil, then the patterns continue where they left off from
the previous repetition.

	
    (section foo :bars 4)
    
    (qball foo-piano piano
       :bars 1
       :reset-on-repeat t
       :cue '((1 1 1)(1 2 1)(1 3 1)(1 4 1))   ;; 4-events
       :key '(c3 e3 g3)                       ;; 3-keys
       :amp '(f mf mp p pp))                  ;; 5-dynamic levels
    
    foo-piano repeats same pattern on each repetition
       (1 1 1) c3 f
       (1 2 1) e3 mf
       (1 3 1) g3 mp
       (1 4 1) c3 p
       (2 1 1) c3 f   ;; repeat at bar 2.
       (2 2 1) e3 mf
    
    (qball foo-organ
      :bars 1
      :reset-on-repeat nil
      :cue '((1 1 1)(1 2 1)(1 3 1)(1 4 1))   ;; 4-events
      :key '(c3 e3 g3)                       ;; 3-keys
      :amp '(f mf mp p pp))                  ;; 5-dynamic levels
    
    foo-organ does not repeat
       (1 1 1) c3 f
       (1 2 1) e3 mf
       (1 3 1) g3 mp
       (1 4 1) c3 p
       (2 1 1) e3 pp   ;; start bar 2, patterns continue
       (2 2 1) g3 f
       (2 3 1) c3 mf
       (2 4 1) e3 mp	




See patterns
See keynumber
See Instrument
See metric-expression
See dynamics

												 
	
