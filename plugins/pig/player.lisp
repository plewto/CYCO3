;;;; cyco3 plugin pig player
;;;;


(param *midi-player* nil)

(defun make-midi-player (id &key (server *pig-server*))
  (let ((real-id (car (new-operator "MidiPlayer" id :server server))))
    (setf *midi-player* real-id)))

(defun assign-midi-player-id (id)
  (setf *midi-player* id))


(labels ((player-address (command id)
	    (sformat "op/~a/~a" (or id *midi-player*) command)) )

  (defun play (&key id (server *pig-server*))
    (read-response server (send (player-address "play" id) :server server)))


  (defun stop (&key id (server *pig-server*))
    (read-response server (send (player-address "stop" id) :server server)))

  (defun resume (&key id (server *pig-server*))
    (read-response server (send (player-address "resume" id) :server server)))


  (defun is-playing (&key id (server *pig-server*))
    (string->bool (car (read-response server
				      (send (player-address "q-is-playing" id) :server server)))))

  (defun add-media-directory (dirname &key id (server *pig-server*))
    (let ((command (player-address "add-media-directory" id)))
      (read-response server
		     (send command :server server :data (list dirname)))))

  (defun dump-media-list (&key id (server *pig-server*))
    (let ((response (read-response server (send (player-address "q-media-list" id) :server server))))
      (if response
	  (progn 
	    (dolist (item response)
	      (format t "~A~%" item))
	    t))))

  (defun select-media (name &key id (server *pig-server*))
    (let ((command (player-address "select-media" id)))
      (read-response server (send command :server server :data (list name)))))

  (defun clear-media-list (&key id (server *pig-server*))
    (read-response server (send (player-address "clear-media-list" id) :server server))))



