;;;; CYCO pig plugin docs
;;;;

(in-package :pig)


(param *player-docs* "
(STOP)  
    Halt playback

(PLAY &optional name)
    Start playback. 
    If optional name is suplied load corresponding MIDI file.
    If name is reative, load from current projects MIDI directory.

(PIG:LOAD-SMF name)
    Load MIDI file into player.
    name should be an absolute filename for a MIDI file.")

(param *midi-docs* "
The following functions transmit a single MIDI message.
    1 <= channel <= 16
    0 <= data < 128 for all data values

    (PIG:NOTE-OFF (channel key &optional (velocity 0)))
    (PIG:NOTE-ON (channel key &optional (velocity 64)))
    (PIG:POLY-PRESSURE channel key pressure)
    (PIG:CONTROLLER channel controller-number value)
    (PIG:PROGRAM channel program-number)
    (PIG:MONO-PRESSURE channel pressure)

(PIG:BEND channel normal-bend)
    Transmit single pitch bend message, -1.0 <= normal-bend <= +1.0

(PIG:SYSEX bytes)
    Transmnit single SYSEX message. 
    bytes should not include SYSEX or EOX status bytes.")


(param *monitor-docs* "
(PIG:MONITOR-EXCLUDE status flag)
    If flag is non-nil exclude status from the monitor output.

(PIG:MONITOR-ON)
    Enable MIDI monitor.

(PIG:MONITOR-OFF)
    Disable MIDI monitor.

(PIG:MONITOR-LOG filename)
    Open filename as monitor log.

(PIG:MONITOR-CLOSE-LOG)
     Close monitor logfile.")


(param *channel-docs* "
(PIG:IN-CHANNELS (&rest CHANNELS))
    Selects MIDI input channels.

(PIG:OUT-CHANNELS (&rest CHANNELS)
    Selects MIDI output channels.")




(defun ?pig (&optional topic)
  (let ((s (cond ((eq topic :player) *player-docs*)
		 ((eq topic :midi) *midi-docs*)
		 ((eq topic :monitor) *monitor-docs*)
		 ((eq topic :channel) *channel-docs*)
		 ((or (not topic)(eq topic :topics))
		  "Select ?PIG topic  :PLAYER  :MIDI  :MONITOR  :CHANNEL"))))
    (format t "~A~%" s)))
		  
