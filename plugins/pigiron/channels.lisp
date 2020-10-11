;;;; cyco3 pigiron plugin
;;;;



(defun query-channel-mode (id &key (proxy *default-proxy*) delay)
  (let* ((command (sformat "op/~A/q-midi-channel-mode" id))
	 (message (send proxy command))
	 (result (read-osc-response proxy message :delay delay)))
      (if (eq result :ERROR)
	  :ERROR
	(->symbol (car result) :keyword))))


;;; For multi channel mode
;;;

(defun query-midi-channels (id &key (proxy *default-proxy*) delay)
  (let* ((command (sformat "op/~A/q-midi-channels" id))
	 (message (send proxy command))
	 (result (read-osc-response proxy message :delay delay)))
      (if (eq result :ERROR)
	  :ERROR
	result)))

(defun enable-midi-channel (id channel enable &key (proxy *default-proxy*) delay)
  (let* ((command (sformat "op/~A/enable-midi-channel" id))
	 (message (send proxy command (list channel (bool->string enable)))))
    (read-osc-response proxy message :delay delay)))


;;; For single channel mode
;;;

(defun query-midi-channel  (id &key (proxy *default-proxy*) delay)
  (let* ((command (sformat "op/~A/q-midi-channel" id))
	 (message (send proxy command))
	 (result (read-osc-response proxy message :delay delay)))
      (if (eq result :ERROR)
	  :ERROR
	(car result))))

(defun select-midi-channel (id channel &key (proxy *default-proxy*) delay)
  (let* ((command (sformat "op/~A/set-midi-channel" id))
	 (message (send proxy command channel)))
    (read-osc-response proxy message :delay delay)))
