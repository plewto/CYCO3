# CYCO Raw Parts

A **RAW-PART** is a primitive type of *PART* intended as a fall-back when
no other part type does the job.  They are for edge-cases and
generally you'll want to use some more advanced part type.

**(MAKE-RAW-PART name &key section bars beats render-once transposable remarks events)** <br>
**(RAW-PART name &key section bars beats render-once transposable remarks events)** <br>

    Creates new RAW-PART.  The only difference between RAW-PART and
    MAKE-RAW-PART is that the former binds the new part object to the
	symbol name while the later does not.   name should be unquoted for
	RAW-PART and quoted for MAKE-RAW-PART.
	
	
    :section      - defaults to current section of *PROJECT*
    :bars         - bars per phrase, inherits from section.
    :beats        - beats per bar, inherits from section
    :render-once  - boolean, if true do not repeat.
    :transposable - boolean if true this part is subject to transpose and
                    key invert operations.  Raw-parts may not be reversed
                    by retrograde operations.
    :remarks      - Optional remarks text.
    :events       - Nested list of events.
	
Event times are specified in seconds relative to the start of the section.
A RAW-PART does not use time-signature or cue-function to determine event
times, you are on your own.

Events are specified as a nested list of MIDI events

     ((time-1 message-1)
	  (time-2 message-2)
	   ................
	  (time-n message-n))
