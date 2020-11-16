;;;; cyco3 plugin pig channel-operators
;;;;

(defun query-channel-mode (id &key (server *pig-server*))
  (let* ((sub-address (sformat "op/~a/q-midi-channel-mode" id))
	 (result (read-response server (send sub-address :server server :data (list id)))))
    (cond
     ((string= (car result) "SINGLE") :SINGLE)
     ((string= (car result) "MULTI") :MULTI)
     (t :NONE))))


(labels ((parse-channel-list (lst)
	   (let ((data (cyco:split-string (car lst))))
	     (setf data (remove-if #'(lambda (q)(string= q "")) data))
	     (mapcar #'parse-integer data)))
			 
	 (query-single-channel (id server)
	    (let* ((sub-address (sformat "op/~A/q-midi-channel" id))
		   (result (read-response server (send sub-address :server server :data (list id)))))
	      (car (parse-channel-list result))))

	 (query-multi-channel (id server)
	    (let* ((sub-address (sformat "op/~A/q-midi-channels" id))
		   (result (read-response server (send sub-address :server server :data (list id)))))
	      (parse-channel-list result))))

  (defun query-midi-channels (id &key (server *pig-server*))
    (let ((mode (query-channel-mode id :server server)))
      (cond
       ((eq mode :SINGLE)(query-single-channel id server))
       ((eq mode :MULTI)(query-multi-channel id server))
       (t :NONE)))))


(labels ((set-single-channel (id channel server)
	    (let* ((sub-address (sformat "op/~a/set-midi-channel" id))
		   (value (if (listp channel) (car channel) channel)))
	      (read-response server (send sub-address :server server :data (list value)))))

	 (set-multi-channels (id channels flag server)
	    (let* ((sub-address (sformat "op/~a/enable-midi-channel" id)))
	      (dolist (channel (->list channels))
		(read-response server (send sub-address :server server :data (list channel flag)))))))

  (defun set-midi-channels (id channels &key (server *pig-server*))
    (let ((mode (query-channel-mode id :server server)))
      (cond
       ((eq mode :SINGLE)(set-single-channel id channels server))
       ((eq mode :MULTI)(set-multi-channels id channels "true" server))
       (t
	(error (sformat "PIG:SET-MIDI-CHANNELS  operator ~S does not support channel selection." id))))))
				   
  (defun clear-midi-channels (id channels &key (server *pig-server*))
    (set-multi-channels id channels "false" server)) )

(defun channel-filter (id &key (server *pig-server*)(channels nil))
  (let ((real-id (car (new-operator "ChannelFilter" id :server server))))
    (if channels
	(set-midi-channels real-id channels :server server))
    real-id))

(defun distributor (id &key (server *pig-server*)(channels nil))
  (let ((real-id (car (new-operator "Distributor" id :server server))))
    (if channels
	(set-midi-channels real-id channels :server server))
    real-id))
			       
				   
			       
