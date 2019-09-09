# CYCO Programs Part

A *PROGRAMS* part is a specialized part for generating instrument program
changes.  Typically they are timed to occur at the start of a
section.

**(MAKE-PROGRAMS name instruments &key time section remarks render-once)** <br>
**(PROGRAMS name instruments &key time section remarks render-once)** <br>

    Creates new PROGRAMS part.  PROGRAMS and MAKE-PROGRAMS are identical
    except the former binds the new part to the symbol name while the later
    does not.  The name argument to MAKE-PROGRAMS should be quoted and
    unquoted for PROGRAMS.
	
	name         - Symbol
	instruments  - List if instruments.
	:time        - Event time, defaults to start of section.
	:section     - Parent section, defaults to current-section of *PROJECT*
	:remarks     - Optional remarks text.
	:render-once - Boolean, if true do not repeat events within the section.
	
Default program change events are generated for each listed instrument. 

