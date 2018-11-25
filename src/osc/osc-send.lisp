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


;; ISSUE: FPO DEBUG REMOVE


(defun ping()
  (osc-send nil :address "/pigiron/ping"))

(defun pigtree ()
  (osc-send nil :address "/pigiron/display-forest"))

(defun piginfo ()
  (osc-send nil :address "/pigiron/channel-filter-1/query-info"))

(defun qtransmitters () ;; all available 
  (osc-send nil :address "/pigiron/query-midi-transmitters"))

(defun qcommands () 
  (osc-send nil :address "/pigiron/channel-filter-1/query-osc-commands"))

(defun enable (flag) ;; ***
  (let ((arg (if flag "TRUE" "FALSE")))
    (osc-send (list arg) :address "/pigiron/channel-filter-1/enable")))

(defun qenabled ()
  (osc-send nil :address "/pigiron/channel-filter-1/query-is-enabled"))

(defun enable-channel (chan flag)
  (let ((arg (if flag "TRUE" "FALSE")))
    (osc-send (list (->string chan) arg)
	      :address "/pigiron/channel-filter-1/enable-channel")))

(defun enable-all ()
  (osc-send nil :address "/pigiron/channel-filter-1/enable-all"))

(defun disable-all ()
  (osc-send nil :address "/pigiron/channel-filter-1/disable-all"))

(defun qchannel (chan)
  (osc-send (list (->string chan))
	    :address "/pigiron/channel-filter-1/query-channel"))

(defun qtransmitter () ;; single op
  (osc-send nil :address "/pigiron/midi-input-1/query-transmitter"))

(defun set-transmitter (tname)
  (osc-send (list (->string tname))
	    :address "/pigiron/midi-input-1/set-transmitter"))

(defun create-operator (optype &key (parent nil)(id nil))
  (osc-send (list (string-downcase (name optype))
		  (if parent
		      (string-downcase (name parent))
		    "NIL")
		  (if id (->string id) "NIL"))
	    :address "/pigiron/create-operator"))
  
