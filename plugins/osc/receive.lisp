;;;; CYCO osc/osc-receive
;;;;
;;;; This is a place holder implementation for testing only.
;;;; It is unlikely OSC reception will be implemented.
;;;; 

(defun osc-receive (&key (port *osc-receive-port*)
			 (host *local-host*)
			 (address *osc-receive-address*)
			 (buffer-length *osc-receive-buffer-length*))
  (declare (ignore address))
  (let ((s (socket-connect nil nil
			   :local-port port
			   :local-host host
			   :protocol :datagram
			   :element-type '(unsigned-byte 8)))
	(buffer (make-sequence '(vector (unsigned-byte 8)) buffer-length)))
    (format t "OSC Listening on localhost port ~A~%~%" port)
    (unwind-protect
	(loop do
	      (socket-receive s buffer (length buffer))
	      (format t "OSC received --> ~S~%" (osc:decode-bundle buffer)))
      (when s (socket-close s)))))
			   
