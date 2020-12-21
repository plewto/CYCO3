;;;; CYCO osc plugin
;;;;

(labels ((format-host
	  (host port address)
	  (sformat "host ~A port ~A address ~S" host port address))

	 (trace-send
	  (host port address)
	  (if *osc-send-verbose*
	      (format t "OSC --> ~A~%" (format-host host port address)))) )

  (defun osc-send (arguments &key 
			     (host *default-osc-send-host*)
			     (port *default-osc-send-port*)
			     (address *default-osc-send-address*))
    "Transmits OSC message.
arguments - List of message arguments.  For convenience a single value may 
            be specified.  Use nil if there are no arguments.
host      - OSC server host address, defaults to *DEFAULT-OSC-SEND-HOST*
port      - OSC server port number, defaults to *DEFAULT-OSC-SEND-PORT*
address   - The OSC address on server, defaults to *DEFAULT-OSC-SEND-ADDRESS*"
    (trace-send host port address)
    (let* ((s (socket-connect host port
  			      :protocol :datagram
  			      :element-type '(unsigned-byte 8)))
	   (cmd (cons address
		      (if (listp arguments)
			  arguments
			(list arguments)))) ;; can not use ->list here
	   (b (apply #'osc:encode-message cmd)))
      (declare (ignore s))
      (unwind-protect
      	  (socket-send s b (length b))
      	(when s (socket-close s))))))
