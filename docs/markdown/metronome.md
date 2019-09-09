# CYCO Metronome Part

The *METRONOME* is a specialized part for creating click tracks.

**(METRONOME name &key ...)**

    Creates metronome part.
	
	name        - part name, the new part is bound to the symbol name.
	:section    - parent section, defaults to current-section of *PROJECT*
	:tempo      - defaults to section tempo
	:unit       - default to section unit
	:bars       - default to section bars
	:beats      - default to section beats
	:cue        - cue pattern, produces an event on every beat.
	:key        - key number pattern.  
                  The *metronome* instrument uses special keynumber mapping and
                  which recognizes 'phrase 'bar and 'beat keys.  phrase is the first
                  beat of the first bar.  bar appears on the first beat of every
                  bar, except for the first bar.  beat appears on all other beats.
	:amp        - dynamic pattern.  
	:cuefn      - default to section cue function
	:instrument - defaults to *METRONOME*


    Metronome is a special case of QBALL.  The default cue list produces an
	event for every beat of every bar.  The :cue keyword argument may be
	used to override the default.
	
