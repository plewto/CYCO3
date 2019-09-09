# CYCO Key Numbers

Within CYCO all integers are valid keynumbers but the effective range is
between 0 and 127 inclusive.  Values above 127 are transposed down by
octaves as needed to be less then 128.  All negative values are treated as
rest with the canonical constant +REST+ equal to -1.

----
## Symbolic Key Numbers.

Symbolic key numbers have the form: 
    
	ROOT[accidental][octave]
	
Where ROOT is one of:

    C D E F G A or B
	
The accidental is optional and may be either:

    F - flat, transpose root key down 1/2 step.
	S - sharp, transpose root key up 1/2 step.
	
The octave is an optional integer between 0 and 10.  If an octave is not 
specified it defaults to 0.

**Examples**

    C5  - middle-c, MIDI keynumber 60.
	AS4 - a-sharp below middle-c, MIDI keynumber 58.
	BF4 - b-flat below middle-c, identical to AS4, keynumber 58.
	
	D - D in octave 0, MIDI keynumber 2.
	
Enharmonic values are defined for b-sharp, c-flat, e-sharp and f-flat.

    B4  = CF5  keynumber 59
    BS4 = C5   keynumber 60
	
    E5  = FF5  keynumber 64
	ES5 = F5   keynumber 65
	
A rest is indicated by the symbol R.

    R = -1
	
CYCO treats all negative keynumbers as a rest.  Functions which manipulate
keynumbers handle negative values properly, and in most cases convert them
to the canonical rest value -1.

    (transpose '(60 -100) 5) --> (65 -1)
	

## Keynumber functions

Most keynumber related functions may be applied to a single keynumber or a
list of keynumbers.  When applied to a list the result is also a list.

**(KEYNUMBER item)**

    Returns keynumber value of item.
	
	item may be an integer, symbolic keynumber or list of keynumbers.
	
	(KEYNUMBER -100) --> -1  a rest
    (KEYNUMBER 'r)   --> -1 
    (KEYNUMBER 60)   --> 60
    (KEYNUMBER 'c4)  --> 48 
	(KEYNUMBER '(20 30 40 r (c4 c5 c6))) --> (20 30 40 -1 (48 60 72))
	
**(KEYNUMBER-P item)**

    Predicate, true if item is a valid keynumber.
	
**(REST-P item)**

    Predicate, true if item is a valid keynumber with a negative value.
	
**(PITCH-CLASS keynumber)**

    Returns integer pitch-class of keynumber.
	The pitch-class is the keynumber mod 12.  All negative values have
    pitch-class of -1.
	
	(PITCH-CLASS 60)   --> 0
	(PITCH-CLASS 'cs7) --> 1
	(PITCH-CLASS -15)  --> -1
	
	PITCH-CLASS may be applied to a list of keynumbers.
	
	(PITCH-CLASS '(20 30 40 r (c4 cs5 d7))) --> (8 6 4 -1 (0 1 2))
	
**(OCTAVE keynumber)**

    Returns octave value of keynumber.  All negative keynumbers have 
	octave -1.
	
	(OCTAVE 60)  --> 5
	(OCTAVE 'c7) --> 7
	(OCTAVE -9)  --> -1
	
	OCTAVE may be applied to list of keynumbers.
	
	(OCTAVE '(60 cs5 (r ds6))) --> (5 5 (-1 6))
	
**(KEYNAME keynumber)**

    Returns the symbolic name of keynumber.
	
	All negative values have name 'R.
	Numeric values above 127 are transposed down by octaves.
	For keynumbers with two enharmonic names, IE CS5 and DF5, the sharp 
	version is returned.
	
	(KEYNAME 'c4)  --> C4
	(KEYNAME -100) --> R
	(KEYNAME 800)  --> GS9, transposed to valid range.
	
	KEYNAME may be applied to list.
	
	(KEYNAME '(C4 -100 800)) --> (C4 R GS9)
	
**(TRANSPOSE object n)**

    Transposes obejct by n half-steps.
	
	TRANSPOSE is a generic-function which may be applied to a wide range
	of types.   For keynumbers TRANSPOSE adds n half-steps.
	
	The return value has the same type as the object argument.
	
	(TRANSPOSE 60 12)   -->  72
	(TRANSPOSE 'C5 12)  -->  C6
	(TRANSPOSE 126 12)  --> 126, automatically transposed back to valid range.
	(TRANSPOSE -100 32) --> -1
	
	(TRANSPOSE '(60 61 62 (C5 CS5 D5)) 12) --> (72 73 74 (C6 CS6 D6))
	

	If the n argument is nil, it is trerated 0.
	
	(TRANSPOSE whatever nil) --> whatever
	
**(INVERT object pivot)**

    Applies key inversion of object around the pivot-key.
	
	INVERT is a generic-function which may be applied to a wide range
	of types.  
	
	
	(INVERT 59 60) --> 61
	(INVERT 60 60) --> 60
	(INVERT 61 60) --> 59
	
	(INVERT 'C4 'C5) --> CS5 
	
	(INVERT '(48 59 60 67 (A3 B4 C7)) 'C5) --> (72 61 60 53 (75 61 36))
	
	A nil pivot value is treated as an identity
	
	(INVERT whatever nil) --> whatever
	
**(DEFKEYNUMBER symbol value)**

    Defines user keynumber.
	
	The value should be an integer between -1 and 127 inclusive.
	The KEYNAME function does not recognize uer-defined key-numbers.
	
	(DEFKEYNUMBER 'MY-REST -1)
	(DEFKEYNUMBER 'ALPHA 34)
	
	(KEYNUMBER 'MY-REST) --> -1
	(KEYNUMBER 'ALPHA)   --> 34
	(KEYNAME 'ALPHA)     --> ALPHA
	(KEYNAME 34)         --> AS2
	
    
	
	
