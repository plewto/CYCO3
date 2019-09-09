# CYCO Dynamics

Numerically CYCO dynamic values are floats less then or equal to 1.0.
All non-positive values are treated as rest.  

## Symbolic Dynamic Values

CYCO symbolic dynamic values are an extension of the common music symbols
P, MF, FF, etc.   These symbols are extended with '-' and '+' suffixes for
higher resolution.   A partial list of dynamic values follows:

    R     = -1.0
	PPPP- ~ 0.016
	PPPP  ~ 0.047
	PPPP+ ~ 0.079
	PPP-  ~ 0.110
	...
	MP-   ~ 0.417
	MP    ~ 0.457
	MP+   ~ 0.489
	MF-   ~ 0.520
	MF    ~ 0.560
	MF+   ~ 0.590
	...
	FFFF- ~ 0.292
	FFFF  ~ 0.969
	FFFF+ = 1.000
	
## Dynamic Functions

Most dynamic functions may take a single value or list of values.

**(DYNAMIC object)**

    Returns numeric dynamic value of object.
	
	(DYNAMIC -100)  --> -1   a rest
	(DYNAMIC 0.5))  --> 0.5
	(DYNAMIC 1.4)   --> 1.0  automatically limited to valid dynamic range.
	(DYNAMIC 'MP)   --> ~0.417
	(DYNAMIC '(0.1 0.2 (MP MF))) --> (0.1 0.2 (0.417 0.560))
	
**(DYNAMIC-P object)**

    Predicate, true is object is a valid dynamic value.
	
**(DYNAMIC-NAME object)**

    Returns symbolic dynamic name for object.  Note there may be slight
    rounding errors and this function horribly inefficient.
	
**(DEFDYNAMIC symbol value)**

    Defines a user dynamic value.
	
**(DYNAMIC->VELOCITY n)**

    Converts dynamic value to integer MIDI velocity.
	0 <= velocity <= 127
	
**(VELOCITY->DYNAMIC velocity)**

    Converts MIDI velocity to dynamic value.
	
