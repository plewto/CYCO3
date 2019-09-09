# CYCO Time Signatures

A **TIME-SIGNATURE** is a type of CYCO-NODE which sets basic timing
parameters.  All composition related classes inherit from TIME-SIGNATURE

    CYCO-NODE
	 |
	 +-- TIME-SIGNATURE
	      |
		  +-- PROJECT
		  +-- SECTION
		  |    |
		  |    + ...
		  |
		  +-- PART
		       |
			   + ...
			   
Briefly a PROJECT is composed of sequential SECTIONs, which are themselves
composed of various PARTs.  This is described in more detail elsewhere.  

A project sets the default time signature which is automatically
inherited by the sections it contains.  The parts in turn inherit the time
signature from their parent sections.   Any section or part may override
the default time parameters.

## Time Signature Parameters

The TIME-SIGNATURE class extends the normal concept of a time-signature
by including the following parameters:

- Tempo - in BPM.
- Beat unit, quarter note by default.
- Bar count, number of bars to a phrase.
- Beat count, number of beats per bar.
- Subbeat count, number of 'sub' beats per beat.  The default value of 4
  sets 4 sixteenth notes per beat.
- Tsubbeat count, number of alternate subbeats per beat.  The default value
  of 3/2 sets 6 sixteenth note triplets per beat.  
  
The following "getter" methods return the various time-signature values.

- **(TEMPO time-signature)**  
- **(UNIT time-signature)**  
- **(BARS time-signature)**  
- **(BEATS time-signature)**  
- **(SUBBEATS time-signature)**  
- **(TSUBBEATS time-signature)**  
- **(TICKS-PER-BEAT time-signature)**  
- **(TICKS-PER-SUBBEAT time-signature)**  
- **(PHRASE-DURATION time-signature)**
- **(BAR-DURATION time-signature)**
- **(BEAT-DURATION time-signature)**
- **(SUBBEAT-DURATION time-signature)**
- **(TBEAT-DURATION time-signature)**
- **(TSUBBEAT-DURATION time-signature)**
- **(TICK-DURATION time-signature)**
  
  
The following "setter" methods update the indicated parameters.

- **(TEMPO! time-signature value)**
- **(UNIT! time-signature value)**
- **(BARS! time-signature value)**
- **(BEATS! time-signature value)**
- **(SUBBEATS! time-signature value)**
