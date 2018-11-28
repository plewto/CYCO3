;;;; *DEPRECIATED*   Replaced with plugins
;;;;
;;;; CYCO
;;;;
;;;;
;;;; Configuration is used to customize CYCO's environment to the user's
;;;; requirements.  The major motivation for configuration is to describe
;;;; the available external MIDI devices.  Configurations use the concept
;;;; of a 'profile' and multiple profiles may exists. 
;;;;
;;;; By default configuration files are located in  ~/.config/cyco/
;;;; with each profile having it's own directory.
;;;; For a profile named 'foo' the main configuration file is
;;;; ~/.config/cyco/foo/config.lisp 
;;;; This file is responsible for loading any additional profile files.
;;;;
;;;; Profile file names should be lower-case.
;;;; See CYCO/config/README for more details.
;;;;  

(cyco-warning "LOCAL-CONFIG has been depreciated")

(setf *cyco-config-directory* (join-path (user-home) ".config/cyco" :as-file))
(setf *cyco-config-profile* "default")
(setf *cyco-config-file* "config.lisp")

(defun load-profile-file (filename)
  "Load file relative to current configuration profile directory."
  (cyco-warning "LOAD-PROFILE-FILE HAS BEEN DEPRECIATED")
  (let ((fqn (join-path *cyco-config-directory*
			*cyco-config-profile*
			filename :as-file)))
    (if (or (probe-file fqn)
	    (probe-file (append-filename-extension fqn ".lisp")))
	(progn
	  (format t "Loading configuration file ~A~%" fqn)
	  (load fqn))
      (cyco-warning
       (sformat "Configuration file ~S does not exists." fqn)))))

(defun load-profile (&optional profile)
  "Loads main profile file.
profile - The profile's name as a synbol or string.  Defaults to 
*cyco-config-profile*"
  (CYCO-WARNING "LOAD-PROFILE has been depreciated")
  (setf *cyco-config-profile*
	(string-downcase (->string (or profile *cyco-config-profile*))))
  (load-profile-file *cyco-config-file*))

(defun load-sub-profile (sub-profile)
  "Loads a configuration profile without making it the default profile.
This allows a new profile to extend an existing profile."
  (cyco-warning "LOAD-SUB-PROFILE has been depreciated")
  (let ((temp *cyco-config-profile*))
    (load-profile sub-profile)
    (setf *cyco-config-profile* temp)))
