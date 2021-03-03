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
     :initform "rcmp"
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

(defun rcmp-proxy (&key (prefix "rcmp")(host #(127 0 0 1))(port 7000))
  (setf *proxy* (make-instance 'rcmp-proxy
				:prefix prefix
				:host host
				:port port))
  *proxy*)

(rcmp-proxy)

(defun format-osc-address (command &optional (proxy *proxy*))
  (sformat "/~A/~A" (rcmp-prefix proxy) command))

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

(defun player-exit (&optional (proxy *proxy*))
  (osc-send "exit" :proxy proxy))

(defun ?player (&optional (proxy *proxy*))
  (osc-send "help" :proxy proxy)
  (format t "~A~%" +DOCS+))

(defun stop (&optional (proxy *proxy*))
  (osc-send "stop" :proxy proxy))

(defun play (&optional (proxy *proxy*))
  (osc-send "play" :proxy proxy))

(defun ?medialist (&optional (proxy *proxy*))
  (osc-send "list" :proxy proxy))

(defun scan-media (directory &optional (proxy *proxy*))
  (osc-send "scan" :proxy proxy :data directory)
  (setf (rcmp-directory proxy) directory)
  (sleep 2))

(defun select-media  (name &optional (proxy *proxy*))
  (osc-send "select" :proxy proxy :data name)
  (setf (rcmp-selected-media proxy) name))

(defun rescan-media (&optional (proxy *proxy*))
  (let ((dir (rcmp-directory proxy))
	(media (rcmp-selected-media proxy)))
    (scan-media dir proxy)
    (select-media media)))
	
(constant +exports+ '(rcmp-proxy
		      *proxy*
		      osc-send
		      player-exit
		      ?player
		      stop
		      play
		      ?medialist
		      scan-media
		      rescan-media
		      select-media))

(constant +cyco-imports+ '(player-exit
			   ?player
			   stop
			   play
			   ?medialist
			   scan-media
			   rescan-media
			   select-media))

(export +exports+ :rcmp)
(import +cyco-imports+ :cyco)

(in-package :cyco)


		      
