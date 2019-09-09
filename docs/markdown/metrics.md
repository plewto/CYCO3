# CYCO Metrics

CYCO *metrics* are relative values used for note duration and delay times.
The basic metric values are:

    W     4    - Whole note
    H     2    - Half
    Q     1    - Quarter
    E     1/2  - Eighth
    S     1/4  - Sixteenth
    T     1/8  - Thirty-second
    X     1/16 - Sixty-fourth
    R     -1   - A rest, has no intrinsic duration.
	
Dotted durations are formed by appending any number of dots "." to the base
symbol.  Each dot scales the value by 3/2.

    Q.     9/4  - dotted quarter note
    Q..    27/8 - double dotted quarter note
    Q...   81/16
	
Triplets are formed by appending any number of t's.  Each t scale the value
by 2/3.

    QT    2/3 - quarter note triplet
    QTT   4/9 
    QTTT  8/27
	
Combining dot and t modifications is legal but pointless

    QT.  1  The t and dot cancel each other.
	
It is also legal, but pointless, to modify the rest symbol R with dot and t
scalars.  A rest has no intrinsic duration and such modifications have no
effect.

----
## Metric Functions

**(METRIC-P object)**

    Predicate, true if object is a valid metric value.
	
**(METRIC object)**

    Converts object to it's metric value.
	
	(METRIC 3.4)  --> 3.4
    (METRIC -123) --> -1  All negative values treated as rest=-1.
    (METRIC 'Q)   --> 1.0 quarter note.
    (METRIC 'E.)  --> 0.75 dotted eighth note.
    (METRIC 'ST)  --> ~0.1667 sixteenth note triplet.
	
	METRIC may be applied to list.
	
	(METRIC '(W H (Q E))) --> (4.0 2.0 (1.0 0.5))
	
----
## Metric Expressions

A *metric-expression* allows complex durations to be specified by combining
the basic metric values above.  The general expression form is:

    [s*]a[-|+b][-|+c]...
	
    Elements in square brackets are optional.
	-|+ indicates either a minus or plus sign.
	
	[s*] is an optional numeric scalar applied to the expression to it's
	     right. 

    a, b & c are basic metric values as described above.
	
**Examples**

    W+Q    --> Add whole and quarter note durations.
	W-S.   --> Subtract dotted sixteenth note from whole note.
	W+Q+S  --> Add whole, quarter and sixteenth notes.
	
	The optional scaling factor is applied -AFTER- all addition and
	subtractions. 
	
	3*W+S  --> First add sixteenth note to whole note. Then scale the 
	           sum by 3.0
			   
**(METRIC-EXPRESSION expression)**

    Evaluate the metric-expression and return numeric result.
	
**(METRIC-EXPRESSION-P object)**

    Predicate, true if object is a valid metric expression.
	
	Note: To determine if object is a metric-expression an attempt is made
	at evaluating it, which is relatively expensive.   METRIC-EXPRESSION-P 
	returns either nil or the metric-expression value of object.
	
	

	
	
