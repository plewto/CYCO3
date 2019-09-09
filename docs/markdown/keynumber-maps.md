# CYCO Keynumber Maps

A *keynumber-map* is a function which maps it's arguments to MIDI key
numbers.

    (lambda kn) --> integer
	
The result should be an integer between -1 and 127 inclusive with negative
result indicating a rest.  A few special cases for the kn argument are
defined.

- :DOC the function should display documentation and return -1.
- Unrecognized arguments should not be treated as an error, instead the
  function should return -1.
  
The following functions generate keynumber-maps of various forms. <br>

**(BASIC-KEYNUMBER-MAP &key min max transpose)**

    Creates a basic keynumber-map 
	
	:min       - integer, keynumbers below min return +REST+, default 0.
	:max       - integer, keynumber above max return +REST+, default 127.
	:transpose - integer, transposition in half-steps.
	             The transposition is applied after the range test, default 0.

**(CIRCULAR-KEYNUMBER-MAP start end)**

    Creates map where keynumbers between start and end are returned in a
	circular manner.
	
	Circular maps are useful for certain percussion instruments such as
	shakers.
	
	A circular map foo where start=10 and end=12 has the following
	response. 
	
	(foo 0) --> 10
	(foo 1) --> 11
	(foo 2) --> 12
	(foo 3) --> 10
	(foo 'whatever) --> -1
	
**(SYMBOLIC-KEYNUMBER-MAP assignments)**

    Creates a keynumber-map using symbolic names.
	
	Symbolic keynumber maps are useful for selecting variations of
	percussion instruments.
	
	The assignments is an association list of form 
	
	((symbol-1 keynumber-1 [optional remarks])
	 (symbol-2 keynumber-2 [optional remarks])
      .......................................
     (symbol-n keynumber-n [optional remarks]))
	 
	A typical map for a snare might be specified as:
	
	(SYMBOLIC-KEYNUMBER-MAP '((hit 36)(rim 38)(flam 35)(edge 37)))
	
    This map has the following behavior:
	
	   (foo 'hit) --> 36
	   (foo 'rim) --> 38
	    ........
	   (foo undefined-symbol) --> -1
	  
    For numeric arguments the map acts like a circular-keynumber-map
	 
	   (foo 0) --> 36
	   (foo 1) --> 38
	   (foo 3) --> 37
	   (foo 4) --> 36
	   
**(METRONOME-KEYNNUMBER-MAP &key phrase bar beat)**

    Creates keynumber-map for use with metronomes.
	
	The metronome map is a specialized form of symbolic-keynumber-map 
	which recognizes the following three symbols.
	
	PHRASE - occurs when the phrase repeats, IE the first beat of bar 1.
	BAR    - occurs on the first beat of each bar, with exception of bar 1.
	BEAT   - occurs on all other beats.
