;;;; pig plugin docs
;;;;

(constant +pig-docs+ "
The PIG plugin provides an interface to the Pigiron MIDI utility,
available at github.com/plewto/Pigiron.

Global variables:

    *pig-sleep-time*         - Time delay after stopping MIDI file player.
    *pig-prefix*             - OSC prefix for the Pigiron server, default /pig
    *pig-host*               - Pigiron host address, defaults to local machine, #(127 0 0 1)
    *pig-port*               - Pigiron port number, default 8020.
    *pig-output-channels*    - List of output MIDI channels.
    *pig-midi-filename*      - Current MIDI file name.
    *pig-player-op*          - Pigiron name for MIDIPlayer operator, default 'player'
    *pig-output-op*          - Pigiron name for main MIDIOutput operator, default 'out'
    *pig-distributor-op*     - Pigiron name form Distributor operator, default 'dist'


The Pigiron server is assumed to have a process graph similar to the following with 
the appropriate Operator names.


    MIDIInput -> ChannelFilter -> Distributor -> Monitor --+--> MIDIOutput
                                                           |
                                              MIDIPlayer --+


The MIDIInput, ChannelFilter and Monitor operators are not directly referenced by the
PIG plugin.


The available functions are:

    (PIG-SEND command &optional (data 0))
        
        Send command to Pigiron server, the data value must not be nil.

    (PIG-PING)

        Connectivity test between CYCO and Pigiron.   Upon execution Pigiron should 
        print something like '/pig/ping' to it's terminal.


    (PIG-LOAD-SMF name)

        Load MIDI file into MIDI file player.
        Name may be absolute filename or relative to the current project.
        For relative names, do not include filename extension.

    (STOP)

        Stop Pigiron MIDI file playback.   CYCO execution stops for *pig-sleep-time* 
        seconds.

    (s)
        A convenience alias for (STOP)


    (PLAY &optional name)

        Start playback of MIDI file.   If name is supplied load the indicated file first
        using (PIG-LOAD-SMF).   Current playback is stopped and CYCO halts for a moment 
        prior to commencing new playback.

   (P &optional name)

       A convenience alias for (PLAY name)
    
   (PLAY-MAIN)

       Play project's main MIDI file.

   (PLAY-SECTION &optional s)

       Play section's most recent MIDI file.
       Section defaults to the project's current-section.
     
   (OUTPUT-CHANNEL c)
   (PIGOUT c)

      OUTPUT-CHANNEL and PIGOUT are identical.
      Sets output MIDI channel(s). 
      This command is useful for setting the destination channels for any MIDI inputs.
      The c argument may be an explicit MIDI channel or a CYCO instrument.   Multiple 
      channels may be selected by using a list.

      The MIDIPlayer is not effected.

   
   (PIG-NOTES keylist &key (vel 64)(dur 1.0))

        Audition indicated notes on the current MIDI output channels.


   (PIG-CC  controller value)

        Transmits MIDI control-change events on current output channels.


   (PIG-program number)

        Transmits MIDI program-change on current output channels.  


   (PIG-BEND value)
      
       Transmits Pitch bend on current output channels.  The value is normalized float
       between -1.0 and +1.0.


   (PIG-SYSEX bytes &optional raw)
       Transmits system-exclusive message.
       Normally, a SYSEX and END-OF-SYSEX status bytes are sent before and after the 
       byte list.   Set the raw argument t to suppress these bytes.
")


(defun ?pig ()
  (format t +pig-docs+))
	  
       
      
     
       



	  

