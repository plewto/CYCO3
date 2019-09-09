
# CYCO Patterns

A **PATTERN** is an object which returns a sequence of values in some
prescribed manner.  Most patterns may be nested to any depth.  The two most
basic patterns are the *cycle* and *line*.

A **CYCLE** returns it's elements in a cyclical manner.

    (CYCLE :of '(A B C)) --> A B C A B C A B...
	
A **LINE** returns it's elements in sequence.  Once all elements have been
returned it continues to return the final element.

    (LINE :of '(A B C)) --> A B C C C...

----
## Functions on Patterns

**(NEXT-1 pattern)**

    Returns the next pattern value.

    NEXT-1 is called recursively on nested patterns.
	
	
	(param foo (cycle :of '(A B C)))
	(next-1 foo) --> A
	(next-1 foo) --> B
	
**(NEXT-N pattern n)**

    Returns the next n values from pattern as a list.
	
	(param foo (cycle :of '(A B C D)))
	(next-1 foo)   --> A
	(next-n foo 2) --> (B C)
	(next-1 foo)   --> D
	
**(NEXT pattern &optional n)**

    By default returns the next value from pattern (as per NEXT-1).
	If n is a positive integer, returns the next n values as a list (as per NEXT-N).
	If n is the keyword :ALL, returns all elements of the pattern.
	If n is the keyword :REST, returns list of all remaining values.
	
    The :ALL and :REST options do not have obvious meanings for some pattern 
	types.  In such cases the results of :ALL and :REST are somewhat arbitrary. 
	
**(VALUE pattern)**

    Returns the current value of pattern.
	
	The current value is the last value produced by calling (NEXT-1 pattern).
	
	(param foo (cycle :of '(A B C)))
	(next-1 foo)   --> A
	(next-1 foo)   --> B
	(value foo)    --> B
	
**(RESET pattern)**

    Restore pattern, and all nested patterns, to their initial state.
	
	(param foo (cycle :of '(A B C)))
	(next-1 foo) --> A
	(next-1 foo) --> B
	(reset foo)
	(next-1 foo) --> A
	
**(PATTERN-P object)**

    Predicate, true if object is any pattern type.
	
**(CARDINALITY pattern)**

    Returns number of elements in the pattern.
	Cardinality is not well defined for all pattern types.
	
**(REMAINING pattern)**

    Returns list of elements remaining to be produced from pattern.
	Remaining is not well defined for all pattern types.
	
**(->PATTERN object &key ptype)**

    Coerce object to a pattern.
	
	If (PATTERN-P object) is true, return object.
	Otherwise return pattern of type ptype (default cycle) with object as 
	an element.  ptype is restricted to the following pattern types:
	 
	- list
	- cycle
	- bag
	- dice
	- coin
	
	
**(TRANSPOSE pattern n)**

    Apply n-step key transposition to pattern elements.
    Transpose is not defined for all pattern type.  Where undefined the 
	pattern is unaltered.
	
**(INVERT pattern pivot)**

    Apply key-inversion around pivot key on all pattern elements.
	Invert is not defined for all pattern types.  Where undefined the
	pattern is unaltered.
	
**(RETROGRADE pattern)**

    Reverse order of pattern elements.
	Retrograde is not defined for all pattern types.  Where undefined the
	pattern is unaltered.
	
**(CLONE pattern)**

	Returns a deep copy of pattern.
	
----
## Pattern Types
	
**(LINE &key of)**
 
    Creates new LINE pattern.

    A LINE returns it's elements in sequence util the final element is reached, 
    thereafter it returns the final value indefinitely.

    (param foo (line :of '(A B C)))
    (next foo 8) --> A B C C C C C C...

**(CYCLE &key of)**

    Creates new CYCLE pattern.

    A CYCLE returns it's elements in sequence.  Once the final element has been 
    reached the cycle repeats.

    (param foo (cycle :of '(A B C)))
    (next foo 8) --> A B C A B C A B...

**(BAG &key of final)**

    Creates new BAG pattern.

    A BAG returns it's elements without replacement, once all elements have been 
    returned a bag returns the final element indefinitely. 

    (param foo (bag :of '(A B C) :final 'end)))
    (next foo 8) --> B A C END END END END END

    The final element my itself be a pattern.

    (param foo (bag :of '(A B C) :final (cycle :of '(ape bat)))
    (next foo 8) --> C A B APE BAT APE BAT APE

**(DICE &key of)**

    Creates new DICE pattern.

    A DICE returns it's elements randomly with replacement.

    (param foo (dice :of '(A B C)))
    (next foo 8) --> B C B A C C A B

**(COIN &key (p 0.5)(head #'true)(tail #'false)(period nil))**

    Creates new COIN pattern.

    A COIN is like a two-sided dice but with the ability to call functions.
    
    :P      - Probability of 'head' result.  0 <= p <= 1, default 0.5.
    :HEAD   - The 'head' value, default #'TRUE.
    :TAIL   - The 'tail' value, default #'FALSE.
    :PERIOD - Sets an arbitrary cardnality.

    The head and tail parameters may be any of the following types:

      - literal value.
      - pattern, apply NEXT-1 when selected.
      - function, call when selected.

    The default behavior returns nil or t with equal probability.

    (param foo (coin))
    (next foo 10) --> T T NIL T T NIL NIL NIL T NIL

    The RETROGRADE method swaps head/tail probability.
    
    (param foo (coin :p 0.75)) ;; head probability 75%
    (retrograde foo)           ;; head probability now 50%

 **(WRAPPER &key (of #'identity)(period 16))**

    Creates new WRAPPER pattern.

    A WRAPPER allows a function to be treated as a pattern.  The function should
    take a single integer argument and has no prescribed return type.  With each 
    call ti NEXT-1 the function is called with an increasing integer argument.
    Once period values have been produced the count begins over at 0.

    Using wrapper to simulate a cycle.

    (param foo (wrapper :of #'(lambda (n)(* n 2)) :period 4))
    (next foo 10) --> 0 2 4 6 0 2 4 6 0 2 
     
**(WALKER &key of)**

    Creates new WALKER pattern.

    A WALKER produces a random walk over it's elements.

    (param foo (walker :of '(A B C D)))
    (next foo 8) --> D C B A B C B A

---
## Nested Patterns

Most pattern types may be nested to any depth.  The NEXT-N method is applied recursively to nested patterns. 

    ;; A LINE with nested CYCLE.
    ;;
    (param foo (line :of (list 'A 'B 'C (cycle :of '(1 2 3)))))
    (next foo 10) --> A B C 1 2 3 1 2 3 1...  
    
    ;; A CYCLE with nested LINE.
    ;;
    (param foo (cycle :of (list 'A 'B (line :of '(1 2 3)))))
    (next foo 15) --> A B 1 A B 2 A B 3 A B 3 A B 3...

    ;; A DICE of CYCLE and LINE.
    ;;
    (param foo (dice :of (list (cycle :of '(A B C))(line :of '(1 2 3)))))
    (next foo 16) --> 1 2 A 3 B C 3 A 3 B C 3 A B 3...
