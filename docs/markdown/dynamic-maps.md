# CYCO Dynamic Maps

A *dynamic-map* is a function which defines an instrument's dynamic
response.  The general form is:

    (lambda dynamic-value) --> float
	
**(BASIC-DYNAMIC-MAP &key scale min max)**

    Returns simple dynamic-map.
	
	:scale - float, scaling factor, default 1.0 
	:min   - float, minimum value, default 0.0
	:max   - float, maximum value, default 1.0
	
	The scaling factor is applied prior to the range test.
	
**(METRONOME-DYNAMIC-MAP &key phrase bar beat)**

    Returns a dynamic-map for use with metronomes.  The map recognizes
	three event types.
	
	  'PHRASE - first beat of bar 1.
	  'BAR    - first beat of all bars, except the first bar.
	  'BEAT   - all other beats.
	  
**\+DEFAULT-DYNAMIC-MAP\+**	  
	
	The dynamic map used by default for all new instruments.
	
