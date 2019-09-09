# CYCO Controllers Part

A *CONTROLLERS* part is used to create MIDI controller, bend and
channel-pressure events.   

**(MAKE-CONTROLLERS name instruments &key ...)** <br>
**(CONTROLLERS name instruments &key ...)**

    Creates CONTROLLERS part.  CONTROLLERS and MAKE-CONTROLLERS are
    identical except the former binds the new part to the symbol name while
    the later does not.  name should be quoted for MAKE-CONTROLLERS and
	unquoted for CONTROLLERS
	
	NAME         - Symbol
    INSTRUMENTS  - List of instruments or single instrument.
    :SECTION     - Parent instrument, defaults to current section of *PROJECT*
    :CUEFN       - Cuing function, defaults to section value
    :SHIFT       - Float, time offset in seconds
    :TEMPO       - Float, tempo in BPM, defaults to section value
    :UNIT        - Symbol, time-signature unit, defaults to section value.
    :BARS        - Integer, bar count, defaults to section value.
    :BEATS       - Integer, beats per bar, defaults to section value.
    :SUBBEATS    - Integer, subbeats per beat, defaults to section value.
    :RENDER-once - Boolean, if true rendered events are not repeated within 
                   section.
    :CURVE       - Function used to alter curve shape.  It should take and return
                   signed 'normalized' floats.  (lambda x) --> y
                   -1.0 <= x <= +1.0,  -1.0 <= y <= +1.0
                   Defaults to #'identity
    :REMARKS     - String, optional remarks text
    :EVENTS      - List of event specifications, see below.


    Events are specified as a nested list of event 'clauses'.  Each clause
	begins with a keyword command followed by a prescribed number of
    arguments. 
	
	The available commands are:
	
	:TIME t1 t2
	
	      Sets time range for events.  Initial time t1 and final time t2
          must be in a format expected by the cue function.   For the
	      default BAR function this is (BAR BEAT SUBBEAT)
		  
		  The special values *1 and *2 indicate the previously specified t1
	      and t2 value respectively. 
	
	:TIME-TO t3
	
	      Shortcut for setting the time range to *2 t3.
		   
		  :TIME-TO t3 is equivalent to  :TIME t1 t2 :TIME t2 t3 
	
	:VALUE v1 v2
	
	     Set initial and final curve values at times t1 and t2
	     respectively.   Values are 'normalized' floats 0.0 <= value <= +1.0
		 For pitch bend values are signed  -1.0 <= bend-value <= +1.0
	
	:VALUE-TO v3
	
	     Shortcut for setting value range to v2 v3
		 :VALUE-TO v3 is equivalent to :VALUE v1 v2 :VALUE v2 v3
	
	:STEPS n
	
	     Number of events between t1 and t2.   2 <= n <= 128
	
	:TYPE v
	
	     Event type may be one of the following.
		 
		 bend     - pitch bend 
		 pressure - channel pressure
		 integer  - integer n indicates controller number n
		 other    - Any other symbol is treated as a MIDI controller name.
		            See GET-CONTROLLER-NUMBER
					
					
