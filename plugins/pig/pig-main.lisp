;;;; CYCO pig plugin
;;;;
;;;; CYCO interface to pigiron
;;;; Pigiron is a MIDI routing utility with integrated MIDI file player,
;;;; available at github.com/plewto/Pigiron
;;;;
;;;; Use function pig:server-proxy to set Pigiron server parameters.
;;;; By default the server values are:
;;;;
;;;;    osc-prefix   /pig
;;;;    host         #(127 0 0 1)
;;;;    port         8020
;;;;
;;;;
;;;;
;;;; Assumed Pigiron MIDI process graph
;;;;
;;;;     --> ChannelFilter --> Distributor --+--> MIDIOutput --> Monitor
;;;;                            MIDIPlayer --+         
;;;;
;;;;
;;;;  Lisp Var        Opertator      Name
;;;;  *midi-player*   MIDIPlayer     player
;;;;  *midi-op*       MIDIOutput     out
;;;;  *monitor*       Monitor        mon
;;;;  *filter*        ChannelFilter  filter
;;;;  *distributor*   Distributor    dist
;;;;
;;;; Usage from CYCO package
;;;;
;;;;   (play &optional name)
;;;;       Optionally load MIDI file and begin playback.
;;;;       If name is relative load it from the current-project's
;;;;       MIDI directory.
;;;;
;;;;   (stop)
;;;;       Stop MIDI player
;;;;
;;;;   (pig:load-smf name)
;;;;       Load MIDI file into player.
;;;;       If name is relative filename, load from current project's MIDI
;;;;       directory.
;;;;
;;;;   (pig:ping)
;;;;       Send 'ping' message to Pigiron.
;;;;
;;;;   (pig:exit)
;;;;       Send 'exit' message to Pigiron.
;;;;
;;;;   (pig:info name)
;;;;       Display info for named operator.
;;;;       Output appears in Pigiron terminal.
;;;;
;;;;   (pig:q-channels)
;;;;       Display info for filter and distributor operators in the
;;;;       Pigiron terminal.   The display will included enabled
;;;;       input and output MIDI channels.
;;;;
;;;;   (pig:monitor-exclude status flag)
;;;;       Enable/Disable monitoring of specific MIDI status types.
;;;;
;;;;   (pig:monitor-on)
;;;;       Enable Pigiron monitor.
;;;;
;;;;  (pig:monitor-off)
;;;;      Disable Pigiron monitor.
;;;;
;;;;  (pig:in-channels  &rest channels)
;;;;      Select Pigiron MIDI input channels.
;;;;
;;;;  (pig:out-channels &rest channels)
;;;;      Select Pigiron MIDI output channels.
;;;;
;;;;  The following functions transmit single MIDI message via Pigiron's
;;;;  output operator.  All channels in range [1,16] inclusive.
;;;;  All data bytes (with exception for bend) 0 <= data < 0x80.
;;;;      
;;;;  (pig:note-off channel key &optional velocity)
;;;;  (pig:note-on channel key &optional velocity)
;;;;  (pig:poly-pressure channel key pressure)
;;;;  (pig:controller channel controller-number value)
;;;;  (pig:program channel program-number)
;;;;  (pig:mono-pressure channel pressure)
;;;;
;;;;  Bend transmits pitch-bend event.  normal-bend is a float
;;;;  -1.0 <= normal-bend <= +1.0.
;;;;
;;;;  (pig:bend channel normal-bend)
;;;;
;;;;  Transmit System Exclusive message, do not include either
;;;; SYSEX or END-OF-SYSEX status bytes.
;;;;
;;;; (pig:sysex bytes)
;;;;


(defpackage :pig
  (:use :cl)
  (:import-from :cyco
		:param
		:->vector
		:->list
		:->string
		:name
		:constant
		:global
		:load-plugin-file
		:property
		:*project*
		:join-path
		:cyco-error
		:sformat))

(in-package :pig)

(load "~/quicklisp/setup")
(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :osc)
  (ql:quickload :usocket)
  (ql:quickload :uiop))

(defclass server-proxy nil
  ((prefix
    :type string
    :initform "/pig"
    :accessor osc-prefix
    :initarg :prefix)
   (host
    :type vector
    :initform #(127 0 0 1)
    :accessor pig-host
    :initarg :host)
   (port
    :type integer
    :initform 8020
    :accessor pig-port
    :initarg :port)
   (response-file
    :type string
    :initform "~/.config/pigiron/response"
    :accessor response-filename
    :initarg :response-file)))

;; Default server-proxy
;;
(global *server-proxy* nil)


(defun server-proxy (&key (prefix "/pig")(host #(127 0 0 1))(port 8020)
			  (response-file "~/.config/pigiron/response"))
  "(PIG:SERVER-PROXY &key prefix host port)
Sets parameters for Pigiron server.
:key  - OSC address prefix, default /pig
:host - ip address, default #(127 0 0 1)
:port - port number, default 8020"
  (setf *server-proxy* (make-instance 'server-proxy
				    :prefix prefix
				    :host host
				    :port port
				    :response-file response-file))
  *server-proxy*)

(server-proxy)


(flet ((format-address (command)
		       (sformat "~A/~A" (osc-prefix *server-proxy*) command)))

      (defun osc-send (command &optional (data 0))
	;; data can not be empty
	(let* ((address (format-address command))
	       (socket (usocket:socket-connect (pig-host *server-proxy*)
					       (pig-port *server-proxy*)
					       :protocol :datagram
					       :element-type '(unsigned-byte 8)))
	       (message (apply #'osc:encode-message (cons address (list data)))))
	  (unwind-protect
	      (progn 
		(usocket:socket-send socket message (length message))
		(format t "PIG: ~A  data ~A~%" address data)
	    (when socket (usocket:socket-close socket)))))))


(defun bool->str (flag)
  (if flag
      "true"
    "false"))

(load-plugin-file "player")
(load-plugin-file "midi")
(load-plugin-file "monitor")
(load-plugin-file "filter")
(load-plugin-file "distributor")
(load-plugin-file "docs")

(defun ping ()
  "(PIG:PING) Sends ping message to Pigiron server."
  (osc-send "ping"))

(defun exit ()
  "(PIG:EXIT) Sends exit message to Pigiron server."
  (osc-send "exit"))

(defun info (what)
  (osc-send "info" what))

(defun q-channels ()
  (osc-send "info" *filter*)
  (osc-send "info" *distributor*))

(export '(*midi-op*
          *midi-player*
	  *monitor*
	  *filter*
	  *distributor*
	  ?pig
          bend
          controller
          exit
          load-smf
          mono-pressure
          note-off
	  note-on
     	  server-proxy
          ping
          play
	  player-ping
          poly-pressure
          program
          stop
	  monitor-exclude
	  monitor-on
	  monitor-off
	  monitor-log
	  monitor-close-log
          sysex
	  info
	  in-channels
	  out-channels
	  q-channels)
	:pig)

(import '(pig:?pig
	  pig:play
	  pig:stop)
	:cyco)

(in-package :cyco)
