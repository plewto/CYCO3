# CYCO General-MIDI Plugin

The *GENERAL-MIDI* plugin creates CYCO instruments for use with General
MIDI. 

**(MAKE-GENERAL-MIDI-INSTRUMENT name &key ...)**  <br>
**(GENERAL-MIDI-INSTRUMENT name &key ...)** 

    Creates new general-midi instrument.  GENERAL-MIDI-INSTRUMENT and
    MAKE-GENERAL-MIDI-INSTRUMENT are identical except the former binds the
    new instrument to the symbol name while the later doe not.  name should
    be quoted for MAKE-GENERAL-MIDI-INSTRUMENT and unquoted for
    GENERAL-MIDI-INSTRUMENT. 
	
	name       - Symbol
    :program   - Symbol or integer program-number, defaults to name.
                 If an integer is used it must be in range 0,127 inclusive.
                 If program is a symbol it must match an entry in 
                 +GENERAL-MIDI-PROGRAMS+.  The function ?GENERAL-MIDI-PROGRAMS 
                 displays a list of valid program symbols.
    :parent    - nil or instance of Instrument, defaults to *ROOT-INSTRUMENT*
    :transient - bool, If true this instrument is purged form the orchestra
    	         tree by the (PRUNE-ORCHESTRA) function. Default t
    :channel   - MIDI channel, defaults to parent's channel.
    :remarks   - Optional remarks text
    :keynumber-map    - See orchestra/keynumber-map.lisp
    :dynamic-map      - See orchestra/dynamic-map.lisp
    :articulation-map - See orchestra/articulation-map.lisp"
	

**(?GENERAL-MIDI-PROGRAMS)**

    Displays list of general MIDI program names.
	
	
