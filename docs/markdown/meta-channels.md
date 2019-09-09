# CYCO MIDI Channels

**(META-CHANNEL! name channel &optional remarks)**

    Makes symbolic assignment to a MIDI channel.  A meta-channel may be
    defined in terms of another channel so long as they ultimately resolve
    to a 'real' channel number.
	
	name    - symbol
	channel - integer 1 <= channel <= 16
	          symbol, an indirect channel assignment.
	remarks - optional remarks text.
	
	Examples:
	
	(META-CHANNEL! 'oberheim 1)
	(META-CHANNEL! 'drum-machine 10)
	(META-CHANNEL! 'snare 'drum-machine)
	
**(META-CHANNEL channel &optional (resolve nil))**

    Returns value of meta-channel.  By default returns one-level of
    indirection.  If resolve is true, returns actual MIDI channel.  
	
	Using examples above:
	
	(META-CHANNEL 'snare)   --> drum-machine
	(META-CHANNEL 'snare t) --> 10
	
**(CHANNEL-NAME channel)**

    Returns the primary name for MIDI channel.
	
	(CHANNEL-NAME 10) --> drum-machine
	(CHANNEL-NAME 6)  --> 6, unassigned channels return their integer value.
	
**(META-CHANNEL-ASSIGNMENT-P object)**

    Predicate, true if object is a channel assignment.
	

**(?META-CHANNELS)**

    Displays list of channel assignments.
