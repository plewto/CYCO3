;;;; CYCO pigOSC plugin
;;;; Provides OSC interface to Pigiron MIDI file player.
;;;;

(load "~/quicklisp/setup")
(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :osc)
  (ql:quickload :usocket))

(import '(osc:encode-message
	  usocket:socket-connect
	  usocket:socket-send
	  usocket:socket-receive
	  usocket:socket-close)
	:cyco)

(global *pigosc-local-host* #(127 0 0 1))
(global *pigosc-host* *pigosc-local-host*)
(global *pigosc-osc-id* "/pig")
(global *pigosc-player-id* "op/MidiPlayer")
(global *pigosc-port* 6060)
(global *pigosc-verbose* nil)
	  


(labels ((format-trace (host port address)
			 (sformat "host ~A port ~A address ~S" host port address))
	 (trace-send (host port address arguments)
		     (if *pigosc-verbose*
			 (format t "PIGOSC --> ~A  arguments --> ~A~%"
				 (format-trace host port address) arguments)))
	 (project-midi-directory nil
				 (if (project-p *project*)
				     (let* ((parent (property *project* :project-directory))
					    (name (string-downcase (->string (name *project*))))
					    (midi-dir (->string (property *project* :OUTPUT-DIRECTORY))))
				       (join-path parent name midi-dir))
				   nil)) )

  (defun pigosc-send (command arguments &key host port)
    (let* ((full-address (str+ *pigosc-osc-id* "/" command))
	   (-host (or host *pigosc-host*))
	   (-port (or port *pigosc-port*))
	   (socket (socket-connect -host -port
				   :protocol :datagram
				   :element-type '(unsigned-byte 8)))
	   (osc-command (cons full-address
			  (if (listp arguments)
			      arguments
			    (list arguments))))
	   (message (apply #'osc:encode-message osc-command)))
      (dismiss socket)
      (trace-send -host -port full-address arguments)
      (unwind-protect
	  (socket-send socket message (length message))
	(when socket (socket-close socket)))))

  (defun player-add-directory ()
    (let ((directory (project-midi-directory)))
      (if directory
	  (pigosc-send (str+ *pigosc-player-id* "/add-directory") directory))))

  (defun player-add (media-name)
    (let* ((parent-directory (project-midi-directory)))
      (if parent-directory
	  (let ((full-path (str+
			    "file://"
			    (append-filename-extension
			     (join-path parent-directory media-name :as-file)
			     ".mid"))))
	    (pigosc-send (str+ *pigosc-player-id* "/add")
			 (list media-name full-path))))))
  
  ) ;; END LABELS

  
(defun player-ping ()
  (pigosc-send (str+ *pigosc-player-id* "/ping") nil))

(defun player-clear ()
  (pigosc-send (str+ *pigosc-player-id* "/clear") nil))

(defun player-remove (media-name)
  (pigosc-send (str+ *pigosc-player-id* "/remove") media-name))

(defun player-select (media-name)
  (pigosc-send (str+ *pigosc-player-id* "/select") media-name))

(defun play (&optional media-name)
  (if media-name
      (player-select media-name))
  (pigosc-send (str+ *pigosc-player-id* "/play") nil))

(defun play-continue ()
  (pigosc-send (str+ *pigosc-player-id* "/continue") nil))

(defun stop ()
  (pigosc-send (str+ *pigosc-player-id* "/stop") nil))

(defun seek (time)
  (pigosc-send (str+ *pigosc-player-id* "/seek") time))


;;
;; Documentation
;;

(setf (documentation '*pigosc-host* 'variable)
      "IP Address for location of remote Pigiron application. 
Defaults to local host #(127 0 0 1)")

(setf (documentation '*pigosc-osc-id* 'variable)
      "OSC command prefix for remote Pigiron application.")

(setf (documentation '*pigosc-player-id* 'variable)
      "OSC ID for Pigiron MidiPlayer, relative to *PIGOSC-OSC-ID*")

(setf (documentation '*pigosc-port* 'variable)
      "Port number for remote Pigiron application.")

(setf (documentation 'pigosc-send 'function)
      "Transmits OSC message to remote Pigiron application.
command - OSC command relative to *PIGOSC-OSC-ID*
arguments - The arguments to the command, If there are no arguments set to NIL.
host - Pigiron host IP address, defaults to *PIGOSC-HOST*
port - Pigiron port, defaults to *PIGOSC-PORT*")

(setf (documentation 'player-add-directory 'function)
      "Instructs remote MIDI player to add all files from the current project's 
MIDI directory to it's play list.")

(setf (documentation 'palyer-add 'function)
      "Instructs remote MIDI player to add a file from the current project's
MIDI directory to the play list.")

(setf (documentation 'player-ping 'function)
      "Sends 'ping' message to remote MIDI player.")

(setf (documentation 'player-clear 'function)
      "Instructs remote MIDI player to clear the play list.")

(setf (documentation 'player-remove 'function)
      "Instructs remote MIDI player to remove a file from the play list.")

(setf (documentation 'player-select 'function)
      "Selects file from the play list or playback.")

(setf (documentation 'play 'function)
      "Instructs remote MIDI player to commence playback.
If optional media-name is specified it is selected for playback, 
otherwise the currently selected file is used.")

(setf (documentation 'play-continue 'function)
      "Instructs remote MIDI player to continue playback from current position.")

(setf (documentation 'stop 'function)
      "Instructs remote MIDI player to stop playback.")

(setf (documentation 'seek 'function)
      "Sets playback position of remote MIDI player.  
Position time is in seconds.")
