# CYCO MIDI Events

MIDI events are a low-level structure.   Typically they are not handled
directly while creating a score but exists just below the surface.

## Definitions:

A *MIDI EVENT* is a cons of form:

    (cons time message)
	
    Where time is the relative event-time and message is one of the MIDI
    messages defined below.

MIDI events usually form the elements of an *EVENT-LIST* 

    ((time-1 . message-1)
	 (time-2 . message-2)
	  ..................
	 (time-n . message-n))
	 
Prior to use an event-list is sorted, first by time, and then by message
priority.  Each MIDI message type has an associated priority.  When two or
more events occur at the same time, messages with lower priority values
are transmitted before those with higher values.   As a concrete example
note-on messages have a lower priority value then note-off messages.  This
ensures that if a note-on and note-off have the same event time, the
note-on is transmitted before the note-off.   If note-off messages were
transmitted first there would be a high potential for stuck notes.  

## MIDI Message Types

The following functions return appropriate MIDI message objects.

    (MIDI-NOTE-ON  channel-index key-number velocity)
    (MIDI-NOTE-ON channel-index key-number velocity)
    (MIDI-POLY-PRESSURE channel-index key-number value)
    (MIDI-CONTROL-CHANGE channel-index controller-number value)
    (MIDI-CHANNEL-INDEX-PRESSURE channel-index value)
    (MIDI-PROGRAM-CHANGE channel-index program-number)
    (MIDI-PITCH-BEND channel-index lsb msb) * see below
    (MIDI-SYSTEM-EXCLUSIVE 	data)
    (MIDI-END-SYSTEM-EXCLUSIVE)
    (MIDI-META-TEXT text)
    (MIDI-META-COPYRIGHT text)
    (MIDI-META-TRACK-NAME text)
    (MIDI-META-INSTRUMENT-NAME text)
    (MIDI-META-LYRIC text)
    (MIDI-META-CUE text)
    (MIDI-META-MARKER text)
    (MIDI-TEMPO-CHANGE bpm)
    (MIDI-TIME-SIGNATURE num unit)
    (MIDI-KEY-SIGNATURE sf &optional (minor nil))
	
## MIDI Channels

MIDI specifies channel numbers between 1 and 16 inclusive.  However for
actual transmitted range is 0 to 15.  CYCO endeavors to always use the
1-16 range for user facing values, however by necessity the 0 to 15 range is
required behind the scenes.  In order to distinguish these closely related
ranges the following convention is strictly adhered to.

- **CHANNEL-INDEX** refers to the transmitted range 0-15.
- **CHANNEL** the unmodified term *channel* always refers to the range 1-16.
  
**(BEND->MIDI-DATA value)**

    Converts signed normalized bend value to a vector #(lsb msb)

    Where 1.0 <= value <= +1.0
	lsb and msb are least and most significant byte values required by 
	MIDI-PITCH-BEND function.
	
**(MIDI-DATA->BEND lsb msb)**

    Converts MIDI pitch bend data values to signed normalized form.
	
	
