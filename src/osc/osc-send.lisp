;;;; CYCO osc/osc-send
;;;;

(labels ((format-host
	  (host port address)
	  (sformat "host ~A port ~A address ~S" host port address))

	 (trace-send
	  (host port address)
	  (if *osc-send-verbose*
	      (format t "OSC --> ~A~%" (format-host host port address)))) )

  ;; command should be list 
  (defun osc-send (command &key 
			   (host *default-osc-send-host*)
			   (port *default-osc-send-port*)
			   (address *default-osc-send-address*))
    (trace-send host port address)
    (let* ((s (socket-connect host port
  			      :protocol :datagram
  			      :element-type '(unsigned-byte 8)))
	   (cmd (cons address
		      (if (listp command)
			  command
			(list command)))) ;; can not use ->list here
	   (b (apply #'osc:encode-message cmd)))
      (dismiss s)
      (unwind-protect
      	  (socket-send s b (length b))
      	(when s (socket-close s))))))
