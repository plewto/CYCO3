;;;; CYCO osc/osc-receive
;;;;

;; ISSUE: This is a place holder implementation for testing only.
;; Need to implementd
;;   1) OSC address discrimination
;;   2) Dispatch functions
;;   3) Way to externaly exit infinite loop ?
;;

(defun osc-receive (&key (port *osc-receive-port*)
			 (host *local-host*)
			 (address *osc-receive-address*)
			 (buffer-length *osc-receive-buffer-length*))
  (dismiss address)
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
			   
