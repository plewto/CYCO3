



(setf (documentation 'pigiron-proxy 'function)
      "Creates new instance of PIGIRON-PROXY.  
The new object is bound to *DEFAULT-PROXY* and is 
used by default for most pigiron related functions.")

(setf (documentation 'ping 'function)
      "Transmits 'ping' message to Pigiron.
Returns T iff expected response received.

A NIL result indicates Pigiron is not running
or there is a mismatch between host address or port 
number.")


(setf (documentation 'panic 'function)
      "Instructs Pigiron to stop all notes and playback.")

(setf (documentation 'initialize 'function)
      "Instructs Pigiron to delete all Operators.")

(setf (documentation 'refresh 'function)
      "Instructs Pigiron to refresh the GUI.
Typically the GUI is not updated after the reception of an OSC 
message until explicitly told to do so by a refresh command.")


(setf (documentation 'load-configuration 'function)
      "Instructs Pigiron to read a configuration file.")

(setf (documentation 'new-operator 'function)
      "Tells Pigiron to create a new operator.
The operator-type must be a valid operator type. 
Currently (Oct 2020) these are MidiInput, MidiOutput,
ChannelFilter, Distributor, MidiPlayer, VirtualKeyboard,
SysexBridge and Monitor.

The id is a unique tag for the new Operator and is used 
for all future manipulation.   If is is not unique, Pigiron
will generate a variant that is.  

Returns the actual Operator id.")


(setf (documentation 'delete-operator 'function)
      "Instructs Pigiron to delete the Operator matching id.")


(setf (documentation 'connect 'function)
      "Instructs Pigiron to make connection from parent to child.
operators.  Both parent and child must be the id's of existing 
operators.  Further Pigiron will not all the construction of
a circular graph.")


(setf (documentation 'disconnect 'function)
      "Instructs Pigiron to disconnect two operators. 
Both parent and child arguments must be id's for existing
operators.  It is not an error if the two operators are not
currently connected.")

(setf (documentation 'query-roots 'function)
      "Returns list of ids for all Pigiron root operators.")

(setf (documentation 'query-operators 'function)
      "Returns list of ids for all Pigiron operators.")

;; BUGGY
;; (setf (documentation 'query-operator-info 'function)
;;       "Returns information on operator id.")

(setf (documentation 'query-midi-transmitters 'function)
      "Returns list of all MIDI transmitter devices.")

(setf (documentation 'query-midi-receivers 'function)
      "Returns list of all MIDI receiver devices. ")

(setf (documentation 'set-midi-device 'function)
      "Sets the backing MIDI device for Pigiron MidiInput or 
MidiOutput operators.

Use query-midi-transmitters and query-midi-receivers for a list 
of available devices.")

(setf (documentation 'midi-input 'function)
      "Creates new MidiInput operator and optional sets
it's MIDI device.  Returns actual id.")

(setf (documentation 'midi-output 'function)
      "Creates new MidiOutput operator and optional sets
it's MIDI device.  Returns actual id.")

(setf (documentation 'query-channel-mode 'function)
      "Returns the channel assignment mode for a Pigiron operator
The result is one of :NONE :SINGLE or :MULTI")

(setf (documentation 'query-midi-channel 'function)
      "Returns current MIDI channel for SINGLE mode Pigiron operator.")

(setf (documentation 'select-midi-channel 'function)
      "Sets MIDI channel for a SINGLE mode pigiron operator.")

(setf (documentation 'query-midi-channels 'function)
      "Returns list of selected MIDI channels from MULTI mode Pigiron operator.")

(setf (documentation 'enable-midi-channel 'function)
      "Enable/disable MIDI channel for MULTI mode Pigiron operator.")


(setf (documentation 'player-id 'function)
      "Returns, and optionally changes, the Operator id by Pigiron MIDI player.
This id is used by default for all player related functions.")

(setf (documentation 'can-record 'function)
      "Returns true if the MIDI player supports recording.")

(setf (documentation 'is-recording 'function)
      "Returns true if the MIDI player is currently recording.")

(setf (documentation 'is-playing 'function)
      "Returns true if the MIDI player is currently playing.")

(setf (documentation 'current-media 'function)
      "Returns short for the MIDI players currently selected media.")

(setf (documentation 'current-media-URLs 'function)
      "Returns full URLs for MIDI players currently selected media.")

(setf (documentation 'current-media-duration 'function)
      "Returns duration in seconds of the MIDI players currently 
selected  media")

(setf (documentation 'player-position 'function)
      "Returns MIDI players current playback position in seconds.")

(setf (documentation 'player-relative-position 'function)
      "Returns relative playback position of the MIDI player.  
 The result is a float between 0 and 1.")

(setf (documentation 'clear-media-list 'function)
      "Removes all entries from the MIDI players media list.")

(setf (documentation 'add-media-directory 'function)
      "Add all MIDI files in directory to MIDI players media list.")

(setf (documentation 'remove-media 'function)
      "Removes named item from MIDI players media list.")

(setf (documentation 'select-media 'function)
      "Select media for playback from the MIDI players media list.")

(setf (documentation 'stop 'function)
      "Stops MIDI player playback.")
(setf (documentation 'play 'function)
      "Start MIDI player playback from beginning ")

(setf (documentation 'player-continue 'function)
      "Start MIDI player playback from the current position.")

(setf (documentation 'seek 'function)
      "Set MIDI player playback position in seconds.")

(setf (documentation 'media-list 'function)
      "Returns current media list from Pigiron.
The return value is one of
1) :ERROR
2) Flat list of media aliases (short form of url).
3) Nested association list  ((:ALIAS a :URL u :INDEX n) ... )")
