;;;; cyco3 plugin pig midi-ports
;;;;

(defun set-midi-device (operator-id device-name &key (server *pig-server*))
  (let ((sub-address (sformat "op/~a/set-midi-device" operator-id)))
    (read-response server (send sub-address :server server :data (list device-name)))))

(defun midi-input (id &key (device nil)(server *pig-server*))
  (let ((real-id (car (new-operator "MidiInput" id :server server))))
    (if (and real-id device)
	(set-midi-device real-id device :server server))
    real-id))

(defun midi-output (id &key (device nil)(server *pig-server*))
  (let ((real-id (car (new-operator "MidiOutput" id :server server))))
    (if (and real-id device)
	(set-midi-device real-id device :server server))
    real-id))
