# CYCO Cuing and Shuffle Functions.

CYCO takes a functional approach to how event times are specified.   A cue
function may be specified at either the project, section or part level.
All cuing functions must have the form:

    (lambda time-signature time-specification) --> float
	
The time-signature corresponds to the project, section or part where the
cuing function is used.  The time-specification has no pre-defined
format.  Of course an acceptable time-specification must be supplied for
whatever cuing function is currently being used. <br>

The default cue function is #'BAR

**(BAR time-signature time-specification)**

    The time-specification is a list of form 
	
	      (BR BT SB TK)

    Where:
	
	    BR - bar number   1, 2, 3, ... <= (bars time-signature), default 1
		BT - beat number  1, 2, 3, ... <= (beats time-signature), default 1
		SB - subbeat      1, 2, 3, ... <= (subbeats time-signature), default 1
		TK - tick         -/+ n, default 0
		
	All values are optional, the following calls are identical:
	
	    (BAR timesig nil) <-> (BAR timesig '(1)) <-> (BAR timesig '(1 1 1 0))
		
		
	()          - Bar 1 Beat 1.
	(1 2)       - Bar 1 Beat 2.
	(1 2 3)     - Bar 1 Beat 2 third sixteenth note. 
	(1 2 3 -17) - 17 ticks before the 3rd sixteenth note of beat 2,
	
	Triplets are specified by prefixing beat or subbeat values with a 'T'.
	
	(1 T1)   - First beat of bar 1, 'T1' and '1' are equivalent.
	(1 T2)   - Second quarter-note triplet of bar 1.
	(1 2 T3) - Third sixteenth-note triplet after beat 2.
	 
	
	Out of bounds values elicit a warning and are treated as 1.   Other
	cue-functions may treat out of bounds values differently.
	
	The 'T' modifier scales the nominal value by 2/3.  For default
	time-signatures with 4 subbeats per beat a 'T' corresponds to a
	sixteenth note triplet.  However a time-signature may have an unusual
	number of subbeats per beat, such as 5.  I'm not sure what such
	subbeats should be called (perhaps 1/20th notes?)  In any case a T
	modifier of where there are 5-subbeats to the beat would result in
	1/30th of a beat. 
	
	
	
**(FLOAT-BAR time-signature time-specification)**

    FLOAT-BAR is an alternative to BAR where time is specified as fractional 
	beat values.  The time-specification has the form:
	
	     (BR BT)
	
	Where:
	   
	   BR - bar number   1, 2, 3, ... <= (bars time-signature), default 1
	   BT - beat number, float or rational,  BT >= 1.0
	   
----

## Shuffle Functions

A shuffle function provides a time shift for specific
time-specification and is set at project, section or
part levels.   Shuffle functions have the form:

    (lambda time-specification) --> float
	
where time-specification should match the cuing function currently being
used.   

**(NO-SHUFFLE time-specification)**

    The default shuffle function ignores it's arguments and always returns 0.0
	
