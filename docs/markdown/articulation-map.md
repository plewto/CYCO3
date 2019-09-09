# CYCO Articulation Maps

An *articulation-map* is a function which defines an instrument's response
to note duration.   The general form is:

    (lambda metric-expression &key time-scale) --> float
	
	metric-expression - note duration specification.
	time-scale        - scaling factor determined by current tempo.
	
	Returns the note-duration.  A negative result indicates a rest.
	
**(BASIC-ARTICULATION-MAP &key scale min max)**

    Creates a simple articulation-map.
	
	:scale - time scaling factor, default 1.0
	:min   - minimum on time, default 0.0
	:max   - maximum on time, default 1e6 (an arbitrarily large value).
	
**(CONSTANT-ARTICULATION-MAP metric-expression)**

    Creates an articulation-map with a constant result.
	
