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
    :accessor pig-prefix
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
    :initarg :port)))

;; Default server-proxy
;;
(global *server-proxy* nil)


(defun server-proxy (&key (prefix "/pig")(host #(127 0 0 1))(port 8020))
  "(PIG:SERVER-PROXY &key prefix host port)
Sets parameters for Pigiron server.
:key  - OSC address prefix, default /pig
:host - ip address, default #(127 0 0 1)
:port - port number, default 8020"
  (setf *server-proxy* (make-instance 'server-proxy
				    :prefix prefix
				    :host host
				    :port port))
  *server-proxy*)

(server-proxy)


(flet ((format-address (command)
		       (sformat "~A/~A" (pig-prefix *server-proxy*) command)))

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


(load-plugin-file "player")
(load-plugin-file "midi")

(defun ping ()
  "(PIG:PING) Sends ping message to Pigiron server."
  (osc-send "ping"))

(defun exit ()
  "(PIG:EXIT) Sends exit message to Pigiron server."
  (osc-send "exit"))

(export '(*midi-op*
          *midi-player*
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
          poly-pressure
          program
          stop
          sysex)
	:pig)

(import '(pig:*midi-player*
	  pig:play
	  pig:stop)
	:cyco)

(in-package :cyco)
