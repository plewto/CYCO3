# CYCO Instrument Program Maps

A *Program map* is a function for actualizing program changes for an instrument. <br>


Program maps have the form 

    (lambda time &key bank program) --> MIDI event list.
	
The exact return format is not specified other then it be a (possibly empty)
list of MIDI events.  This allows program-maps in general to service a wide
range of situations; everything from simple program changes to complex bank
changes and key-switched instruments.  <br>

A program-map should support two special values for the :program argument.

    :program = :doc     --> Print documentation and return an empty list.
	:program = :default --> Use the instruments program-number property.
	

**(NULL-PROGRAM-MAP time &key bank program)**

    The NULL-PROGRAM-MAP is a place-holder function which ignores all of
	it's arguments and returns an empty list.
	

**(SET-BASIC-PROGRAM-MAP instrument &key (offset 0)(min 0)(max 127))**

    Sets a basic program-map for instrument.
	
	The program-map has a range of program-numbers to which it responds,
	programs outside this range are ignored. 
	
	The offset argument is typically either 0 or 1 to match the lowest
	displayed program of the instrument.  IE the lowest displayed value of
	a Yamaha DX7 is 1, which corresponds to MIDI program number 0.  On the
	other hand the lowest displayed program for an Oberheim Matrix-1000 is 0.
	
	If the map sees the program number :default it uses the instrument's
	program-number property. 
	
**(SET-SYMBOLIC-PROGRAM-MAP instrument assignments &key offset)**

    Assigns a symbolic program-number map to instrument.
	
	assignments argument is either a hash-table or association-list.  The
	assignments have the form:
	
	    ((cons name-1 program-number-1 [optional remarks])
		 (cons name-2 program-number-2 [optional remarks])
		  ................................................)
