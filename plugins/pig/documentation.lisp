;;;; cyco3 plugin pig documentation
;;;;


(setf (documentation 'set-pig-server 'function)
      "Establishes value for OSC interaction with Pigiron.")

(setf (documentation 'ping 'function)
      "Transmits diagnostic 'ping' message to Pigiron.
A non-nil result indicates success.")

(setf (documentation 'panic 'function)
      "Transmits 'panic' message to Pigiron.")

(setf (documentation 'initialize 'function)
      "Instructs Pigiron to delete all operators.")

(setf (documentation 'refresh 'function)
      "Instructs Pigiron to refresh it's GUI.")

(setf (documentation 'query-midi-transmitters 'function)
      "Returns list of MIDI transmitter device names.")

(setf (documentation 'query-midi-receivers 'function)
      "Returns list of MIDI receiver device names.")

(setf (documentation 'query-roots 'function)
      "Returns list of names for all Pigiron root operators.")

(setf (documentation 'query-operators 'function)
      "Returns list of names for all Pigiron operators.")

(setf (documentation 'new-operator 'function)
      "Creates new Pigiron Operator
operator-type must name a valid Pigiron Operator class.
The id argument is a suggestion only.  If an operator already 
exist with the same id, Pigiron will assign a new one.
Returns the actual id.")

(setf (documentation 'delete-operator 'function)
      "Deletes the named Pigiron operator.")

(setf (documentation 'connect 'function)
      "Makes connection between Pigiron parent and child operators.")

(setf (documentation 'chain 'function)
      "Makes connections between list of Pigiron operators.
For chain list of 'A' 'B' 'C',  connections are made from 
A to B and then from B to C.")

(setf (documentation 'disconnect 'function)
      "Breaks connection between Pigiron parent and child operators.")

(setf (documentation 'set-midi-device 'function)
      "Sets the backing MIDI device for given Pigiron operator.
See query-midi-receivers and query-midi-transmitters for 
list of available devices.")

(setf (documentation 'midi-input 'function)
      "Creates Pigiron MidiInput operator.
If device is non-nil set as the backing device.
See query-midi-transmitters.")

(setf (documentation 'midi-output 'function)
      "Creates Pigiron MidiOutput operator.
If device is non-nil set as the backing device.
See query-midi-receivers.")

(setf (documentation 'query-channel-mode 'function)
      "Returns MIDI channel mode for named Pigiron operator.
The result is one of :SINGLE, :MULTI or :NONE")

(setf (documentation 'query-midi-channels 'function)
      "Returns list of enabled MIDI channels for Pigiron operator.")

(setf (documentation 'set-midi-channel 'function)
      "Sets enabled MIDI channel(s) for Pigiron operator.
channel argument may be either a single MIDI channel or a list of channels.
For :SINGLE mode operators only the first channel is used.")

(setf (documentation 'clear-midi-channels 'function)
      "Disables list of MIDI channels for named Pigiron operator.")

(setf (documentation 'channel-filter 'function)
      "Creates Pigiron ChannelFilter operator.  
Returns actual assigned id.")

(setf (documentation 'distributor 'function)
      "Creates Pigiron Distributor operator.
Returns actual assigned id.")

(setf (documentation 'make-pig-player 'function)
      "Instructs Pigiron to create a MidiPlayer operator.
Returns the actual Operator id.")

(setf (documentation 'set-pig-player-id 'function)
      "Sets the OSC id for the Pigiron MidiPlayer.
The player id defaults to 'player' if not explicitly set.")

(setf (documentation 'play 'function)
      "Instructs default Pigiron MidiPlayer to start playback.")

(setf (documentation 'stop 'function)
      "Instructs default Pigiron MidiPlayer to stop playback.")

(setf (documentation 'resume 'function)
      "Instructs default Pigiron MidiPlayer to resume playback from current position.")

(setf (documentation 'is-playing 'function)
      "Returns true if Pigiron MidiPlayer is currently playing.")

(setf (documentation 'add-media-directory 'function)
      "Add directory contents to Pigiron MidiPlayer media list.
The directory name may begin with '~' to indicate the home directory.")

(setf (documentation 'dump-media-list 'function)
      "Displays contents of Pigirons MidiPlayer media list.")

(setf (documentation 'select-media 'function)
      "Selects item from Pigiron MidiPlayer media list for playback.")

(setf (documentation 'clear-media-list 'function)
      "Clears Pigiron MidiPlayer media list.")
