;;;; CYCO rcmp plugin
;;;; CYCO interface to rcmp MIDI file player.
;;;; The rcmp player may be obtained from https://github.com/plewto/rcmp
;;;;

(defpackage :rcmp
  (:use :cl)
  (:import-from :cyco
		:constant
		:global
		:load-plugin-file
		:sformat))

(in-package :rcmp)

(load-plugin-file 'docs)


(load "~/quicklisp/setup")
(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :osc)
  (ql:quickload :usocket)
  (ql:quickload :uiop))

(defclass rcmp-proxy nil
  ((prefix
     :initform "/rcmp"
     :accessor rcmp-prefix
     :initarg :prefix)
    (host
     :initform #(127 0 0 1)
     :accessor rcmp-host
     :initarg :host)
    (port
     :initform 7000
     :accessor rcmp-port
     :initarg :port)
    (current-directory
     :initform ""
     :accessor rcmp-directory
     :initarg :dir)
    (selected-media
     :initform ""
     :accessor rcmp-selected-media
     :initarg :file)))

(global *proxy* nil)

(defun rcmp-proxy (&key (prefix "/rcmp")(host #(127 0 0 1))(port 7000))
  "Create new instance of RCMP-PROXY and bind it to the symbol rcmp:*PROXY*
Most functions in the rcmp package use *proxy* by default.

(rcmp:RCMP-PROXY &key prefix host port)

:prefix - OSC message path prefix, default rcmp
:host   - ip address for rcmp server, default #(127 0 0 1)
:port   - server port number, default 7000"

  (setf *proxy* (make-instance 'rcmp-proxy
				:prefix prefix
				:host host
				:port port))
  *proxy*)

(rcmp-proxy)

(defun format-osc-address (command &optional (proxy *proxy*))
  (sformat "~A/~A" (rcmp-prefix proxy) command))

(defun osc-send (command &key (proxy *proxy*)(data nil))
  (let* ((socket (usocket:socket-connect (rcmp-host proxy)
					 (rcmp-port proxy)
					 :protocol :datagram
					 :element-type '(unsigned-byte 8)))
	 (message (apply #'osc:encode-message (cons (format-osc-address command proxy)
						    (list data)))))
    (unwind-protect
	(usocket:socket-send socket message (length message))
      (when socket (usocket:socket-close socket)))))

(defun exit (&optional (proxy *proxy*))
  "Command rcmp program to terminate."
  (osc-send "exit" :proxy proxy))

(defun help (&optional (proxy *proxy*))
  "Display documentation about the rcmp plugin."
  (osc-send "help" :proxy proxy)
  (format t "~A~%" +DOCS+))

(defun stop (&optional (proxy *proxy*))
  "Command rcmp player to halt playback."
  (osc-send "stop" :proxy proxy))

(defun play (&optional (proxy *proxy*))
  "Command rcmp player to start playback of the currently selected MIDI file."
  (osc-send "play" :proxy proxy))

(defun ?media (&optional (proxy *proxy*))
  "Command rcmp to display the media-list."
  (osc-send "list" :proxy proxy))

(defun scan (directory &optional (proxy *proxy*))
  "Command rcmp to scan directory for MIDI files.
The media-list is first cleared, then all MIDI files in directory 
are added to the list.  The first file is automatically selected."
  (osc-send "scan" :proxy proxy :data directory)
  (setf (rcmp-directory proxy) directory)
  (sleep 2))

(defun select  (name &optional (proxy *proxy*))
  "Select file from rcmp media-list.  name may either be the integer position 
of the file in the media-list, or it's actual name."
  (osc-send "select" :proxy proxy :data name)
  (setf (rcmp-selected-media proxy) name))

(defun rescan (&optional (proxy *proxy*))
  "Clear media-list and rescan the current directory."
  (let ((dir (rcmp-directory proxy))
	(media (rcmp-selected-media proxy)))
    (scan dir proxy)
    (select media)))
	
(constant +exports+ '(rcmp-proxy
		      *proxy*
		      exit
		      help
		      stop
		      play
		      ?media
		      scan
		      rescan
		      select))

(export +exports+ :rcmp)


(in-package :cyco)


		      
