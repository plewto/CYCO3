;;;; cyco3 antipig plugin
;;;;
;;;; Send commands to Pigiron via intermediate Python program.
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

(defun pig-send (command &rest args)
  "Sends command to Pigiron via external program.
All arguments must Strings."
  (let ((handler (resolve-user-home *antipig-osc-handler*)))
    (sb-ext:run-program handler (cons command args) :output t)))

(defun pig-ping ()
  "Transmits test 'ping' to Pigiron."
  (pig-send "ping"))

;;
;; MIDI Input channel
;;

(defun pigin (channel)
  "Selects MIDI input channel.
If channel is :OFF, MIDI input is disabled."
  (if (eq channel :off)
      (pig-send "midi-off")
    (pig-send "input-channel" (->string (channel channel)))))

;;
;; MIDI output channels
;;

(defun pigout (&rest channels)
  "Selects MIDI output channels.
Multiple channels may be enabled simultaneously."
  (pig-send "clear-output-channels")
  (loop for c in channels do
	(sleep 0.5)
	(pig-send "add-output-channel" (->string (channel c))))
  (sleep 0.5)
  (pig-send "display-output-channels"))


;;
;; player
;;

(defun pig-load (name)
  "Instructs Pigiron player to load a MIDI file."
  (pig-send "load" (->string name)))

(defun stop ()
  "Halts Pigiron playback."
  (pig-send "stop"))

(defun s ()
  "Shortcut for Pigiron 'STOP' command."
  (stop))

(defun play (&optional name)
  "Starts Pigiron playback."
  (when name
    (pig-load-smf name)
    (sleep 1.0))
  (pig-send "play"))

(defun p (&optional name)
  "Shortcut for Pigiron PLAY command."
  (funcall #'play name))

(defun play-section (&optional sname)
  "Instructs Pigiron to play the current section's MID file."
  (pig-send "play-section" sname))
      
(defun play-main ()
  "Instructs Pigiron to play the current projects main MIDI file."
  (pig-send "play-main"))

;;
;; Monitor
;;

(defun mon-on ()
  "Enables Pigiron monitor."
  (pig-send "mon" "on"))

(defun mon-off ()
  "Disables Pigiron monitor."
  (pig-send "mon" "off"))


(defun ?pig ()
  (format t "ANTIPIG plugin is used for remote control of the Pigiron MIDI routing program~%")
  (format t "~%Required Pigiron configuration:~%~%")
  (format t "    in --> filter --> distributor --+--> out --> mon~%")
  (format t "                                    |~%")
  (format t "                           player --+~%~%")
  (format t "    pig-ping      Transmits diagnostic 'ping' to Pigiron.~%")
  (format t "    pigin         Selects MIDI input channel. If set to :OFF input is disabled.~%")
  (format t "    pigout        Selects MIDI output channels. Multiple channels may be selected at once.~%")
  (format t "    pig-load      Instructs player to load MIDI file.~%")
  (format t "    stop          Stops playback.  The shortcut (s) is identical.~%")
  (format t "    play          Starts playback.  The shortcut (p) is identical.~%")
  (format t "    play-section  Starts playback of the current section.~%")
  (format t "    play-main     Starts playback of the project's main MIDI file.~%")
  (format t "    mon-off       Disables MIDI monitor.~%")
  (format t "    mon-on        Enables MIDI monitor.~%"))
