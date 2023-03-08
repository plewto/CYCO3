;;;; cyco3 antipig plugin
;;;;

(param *ANTIPIG-TEMP-DIRECTORY* "/tmp/antipig/")
(param *ANTIPIG-OSC-HANDLER* "~/cyco3/plugins/antipig/osc-handler.py")

(setf *midi-save-hook*
      #'(lambda (smf-filename)
	  (ensure-directories-exist *ANTIPIG-TEMP-DIRECTORY*)
	  (let ((destination (join-path *ANTIPIG-TEMP-DIRECTORY*
					"current-section-midifile" :as-file)))
	    (when smf-filename
	      (with-open-file (stream destination
				      :direction :output
				      :if-exists :supersede
				      :if-does-not-exist :create)
			      (progn (format stream smf-filename)
				     (format stream "~%")))))))

(setf *make-project-hook*
      #'(lambda (args)
	  (ensure-directories-exist *ANTIPIG-TEMP-DIRECTORY*)
	  (let* ((name (string-downcase (->string (second (assoc :name args)))))
		 (parent (string-downcase (->string (second (assoc :project-directory args)))))
		 (project-directory (join-path parent name))
		 (outdir (->string (second (assoc :output-directory args))))
		 (output-directory (join-path project-directory outdir))
		 (destination (join-path *ANTIPIG-TEMP-DIRECTORY*
					 "midi-directory" :as-file)))
	    (with-open-file (stream destination
				    :direction :output
				    :if-exists :supersede
				    :if-does-not-exist :create)
			    (progn 
			      (format stream output-directory)
			      (format stream "~%")))
	    (setf destination (join-path *ANTIPIG-TEMP-DIRECTORY*
					 "current-project" :as-file))
	    (with-open-file (stream destination
				    :direction :output
				    :if-exists :supersede
				    :if-does-not-exist :create)
			    (progn 
			      (format stream name)
			      (format stream "~%"))))))


(defun antipig-proxy ()
  (resolve-user-home *ANTIPIG-OSC-HANDLER*))

(defun pig-ping ()
  (let ((cmd (antipig-proxy)))
    (sb-ext:run-program cmd '("ping") :output t)))

(defun pig-load-smf (name)
  (let ((cmd (antipig-proxy)))
    (sb-ext:run-program cmd (list "load" (->string name)) :output t)))

(defun stop ()
  (let ((cmd (antipig-proxy)))
    (sb-ext:run-program cmd (list "stop" ) :output t)))

(defun s ()(stop))

(defun play (&optional name)
  (when name
    (progn 
      (pig-load-smf name)
      (sleep 1.0)))
  (let ((cmd (antipig-proxy)))
    (sb-ext:run-program cmd (list "play" ) :output t)))

(defun p (&optional name)
  (funcall #'play name))

(defun pigout (&rest args)
  (when args
    (let ((cmd (antipig-proxy))
	  (acc (list "out-channels")))
      (loop for a in args do (push (->string (channel a)) acc))
      (sb-ext:run-program cmd (reverse acc) :output t))))

(defun pigoff ()
  (let ((cmd (antipig-proxy)))
    (sb-ext:run-program cmd '("off") :output t)))
    
(defun play-main ()
  (let ((cmd (antipig-proxy)))
    (sb-ext:run-program cmd '("play-main") :output t)))

(defun play-section (&optional sname)
  (let ((cmd (antipig-proxy))
	(args (if sname
		  (list "play-section" (string-downcase (->string sname)))
		(list "play-section"))))
    (sb-ext:run-program cmd args :output t)))

(defun mon-on ()
  (let ((cmd (antipig-proxy)))
    (sb-ext:run-program cmd '("mon" "on") :output t)))

(defun mon-off ()
  (let ((cmd (antipig-proxy)))
    (sb-ext:run-program cmd '("mon" "off") :output t)))

(defun pig-debug ()
  (let ((cmd (antipig-proxy)))
    (sb-ext:run-program cmd '("debug") :output t)))

(defun ?pig ()
  (format t "ANTIPIG plugin is used for remote control of the Pigiron MIDI routing program~%")
  (format t "Commands:~%")
  (format t "    (PIG-PING)                     Transmits diagnostic 'ping' message.~%")
  (format t "    (PIG-LOAD-SMF FILENAME)        Loads MIDI file for playback.~%")
  (format t "    (STOP) or (S)                  Stops playback.~%")
  (format t "    (PLAY &OPTIONAL NAME) or (P)   Starts playback.~%")
  (format t "    (PIGOUT CHANNELS...)           Sets MIDI output channels.~%")
  (format t "    (PIGOFF)                       Disables all MIDI output channels.~%")
  (format t "    (PLAY-MAIN)                    Plays project's main MIDI file.~%")
  (format t "    (PLAY-SECTION &OPTIONAL NAME)  Plays current section's MIDI file.~%")
  (format t "    (MON-OFF)                      Disable monitor.~%")
  (format t "    (MON-ON)                       Enables monitor.~%")
  (format t "~%Required Pigiron configuration and:~%~%")
  (format t "    in --> filter --> distributor --+--> out --> mon~%")
  (format t "                                    |~%")
  (format t "                           player --+~%"))
