# CYCO Instruments

An *instrument* is a CYCO-NODE which defines the properties of either an
external MIDI synthesizer or a specific program of an external
synthesizer.  Essentially they are proxies for external
synthesizers/programs. <br>

CYCO does not define an *orchestra* object parse, instead the term
'orchestra' refers to the tree rooted at \*ROOT-INSTRUMENT\* and all
instruments are linked into this tree. <br>


Instruments are roughly divided into two groups:

1. *Non-transient*, permanent instruments representing available external
    MIDI resources, either hardware or virtual.
	
2. *Transient*, project level instruments defined as child nodes of the
   non-transient instruments.   
   
See CYCO-NODE page for explanation of this division.


## Instrument Properties

- program-map
- program-number
- program-bank
- keynumber-map
- dynamic-map
- articulation-map
- MIDI channel

**(INSTRUMENT-P object)**

    Predicate, true if argument is an instrument.
	
**(PROGRAM-MAP! instrument function)**  <br>
**(PROGRAM-MAP instrument)**

    Sets/Retrieves instrument's program map.
    See program-maps
	
**(PROGRAM-NUMBER! instrument program)** <br>
**(PROGRAM-BANK! instrument bank)**      <br>
**(PROGRAM-NUMBER instrument)**          <br>
**(PROGRAM-BANK instrument)**

    These functions set or return an instrument's default program number
	and bank. 
	
**(PROGRAM-CHANGE-EVENTS instrument time &key bank program)**

    Generates list of MIDI events to execute a program change.
	
	Unless overridden by the bank and/or program arguments, the
	instruments default bank/program properties are used.
	
    See program-map
	
**(KEYNUMBER-MAP! instrument function)**  <br>
**(KEYNUMBER-MAP instrument)**

    Sets/Retrieves instrument's keynumber-map function.
	
**(ARTICULATION-MAP! instrument function)**  <br>
**(ARTICULATION-MAP instrument)**

    Sets/Retrieves instrument's articulation-map function.
	
**(DYNAMIC-MAP! instrument function)**  <br>
**(DYNAMIC-MAP instrument)**

    Sets/Retrieves instrument's dynamic-map function.	

**(CHANNEL! instrument channel)**

    Sets instrument's MIDI channel.  
	
	By default an instrument inherits the
	MIDI channel from it's parent.  The channel may either be an absolute
	integer between 1 and 16 or a symbolic meta-channel.

	
**(CHANNEL instrument &optional resolve)**

    Returns the instrument's MIDI channel.
	See "meta-channel" for explanation of resolve argument.
	
**(CHANNEL-INDEX instrument)**

    Returns the instrument's channel-index, an integer between 0 and 15.
	
	See midi-events
	
**\*ROOT-INSTRUMENT\***

    *ROOT-INSTRUMENT* forms the root of the orchestra. 
	
	Ultimately all instruments inherit from *ROOT-INSTRUMENT*
	
**NULL-INSTRUMENT**

    The NULL-INSTRUMENT is a place-holder instrument 
	
	The NULL-INSTRUMENT does not generate any MIDI events.
	
**(MAKE-INSTRUMENT name &key parent transient channel program bank 
                   keynumber-map dynamic-map articulation-map remarks)**

    Creates a new instance of INSTRUMENT
	
	name              - Symbol 
	:parent           - The parent instrument, default *ROOT-INSTRUMENT*
	:transient        - Boolean, set to nil if the instrument is being defined by a project.
	                    default nil.
	:channel          - MIDI channel, defaults to parent channel.
	:program          - Default program number, passed as :program argument to the program-map.
	:bank             - Default program bank, passed as :bank argument to the program-map.
	:keynumber-map    - Function, see keynumber-map
	:dynamic-map      - Function, see dynamic-map
	:articulation-map - Function, see articulation-map
	:remarks          - Optional remarks text.
	

**(INSTRUMENT name &key parent transient channel program bank 
              keynumber-map dynamic-map articulation-map remarks)**

    Macro version of MAKE-INSTRUMENT function.
	
	The only difference between MAKE-INSTRUMENT and INSTRUMENT is that the later
	binds the new instrument object to the symbol name.
