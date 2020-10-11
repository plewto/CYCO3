;;;; cyco3 pigiron io.lisp
;;;;


(defun set-midi-device (id device-name &key (proxy *default-proxy*) delay)
  (let* ((command (sformat "op/~A/set-midi-device" id))
	 (message (send proxy command device-name)))
    (read-osc-response proxy message :delay delay)))
      
	
(defun midi-input (id &key device (proxy *default-proxy*) delay)
  (let ((actual-id (new-operator "MidiInput" id :proxy proxy :delay delay)))
    (sleep 0.1)
    (if device
	(set-midi-device actual-id device :proxy proxy :delay delay))
    (refresh)
    actual-id))


(defun midi-output (id &key device (proxy *default-proxy*) delay)
  (let ((actual-id (new-operator "MidiOutput" id :proxy proxy :delay delay)))
    (sleep 0.1)
    (if device
	(set-midi-device actual-id device :proxy proxy :delay delay))
    (refresh)
    actual-id))
