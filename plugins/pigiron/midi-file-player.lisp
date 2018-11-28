;;;; PigIron Profile MidiFilePlayer
;;;;
;;;;
;;;; query-midi-filename
;;;; query-is-playing
;;;; load-midi-file filename
;;;; transport-play
;;;; transport-stop
;;;;
;;;; NOTE: At this time (2018.11.25) PigIron MIDI file player is
;;;; rudimentary.  Consider these functions provisional.
;;;;

(defun ?pig-midi-filename (&key (op *midi-file-player-id*))
  "Instructs PigIron to display current MIDI file filename.
:op - operator, defaults to *midi-file-player-id*"
  (osc-send nil :address (join-address op 'query-midi-filename)))

(defun ?pig-is-playing (&key (op *midi-file-player-id*))
  "Instructs PigIron to display playing status of MIDI file player.
:op - operator, defaults to *midi-file-player-id*"
  (osc-send nil :address (join-address op 'query-is-playing)))

(defun pig-load-midi-file (filename &key (op *midi-file-player-id*))
  "Instructs PigIron MIDI File player to load a MIDI file.
filename - MIDI file name.
:op - operator, defaults to *midi-file-player-id*"
  (osc-send (list filename)
	    :address (join-address op 'load-midi-file)))

(defun pig-play (&key filename (op *midi-file-player-id*)(delay 2))
  "Instructs PigIron to play MIDI file.
:filename - optional MIDI filename, if specified load file first. Otherwise use previous file.
:op - operator, defaults to *midi-file-player-id*
:delay - time in seconds to wait prior to commencing play, only used if a file is being loaded."
  (if filename
      (progn 
	(pig-load-midi-file filename :op op)
	(sleep delay)))
  (osc-send nil :address (join-address op 'transport-play)))

(defun pig-stop (&key (op *midi-file-player-id*))
  "Instructs PigIron to stop MIDI file playback."
  (osc-send nil :address (join-address op 'transport-stop)))
  

