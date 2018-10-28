;;;; CYCO3 src/local-config
;;;;
;;;; CYCO configuration is used customize it's environment to the user's
;;;; requirements.  The major motivation for configuration is to describe
;;;; the available external MIDI devices,  and more then one set of 
;;;; configurations may exists.
;;;;
;;;; There is no pre-defined structure to the configuration files other
;;;; then the location and name of primary configuration file.  The
;;;; primary file should look after loading any additional configuration
;;;; files.
;;;;
;;;; By default the primary configuration file is
;;;;
;;;;     <user-home>/.config/cyco3/cyco-config.lisp
;;;;
;;;; The folder CYCO3/config contains 2 example configuration schemes.
;;;; The first is a simple scheme based on general MIDI instruments.
;;;; The second, called sj, is more involved and is personal configuration.
;;;;
;;;; Configurations are not automatically loaded when CYCO starts.  Instead
;;;; use the load-config function.  This, together with the SBCl save-snapshot
;;;; feature, allows several executable copies of CYCO to be created with 
;;;; different configuration.
;;;;

(setf *cyco-config-directory* (join-path (user-home) ".config/cyco3" :as-file))
(setf *cyco-config-file* "cyco-config.lisp")

(defun load-config-file (filename)
  "Load file relative to config directory."
  (let ((fqn (join-path *cyco-config-directory* filename :as-file)))
    (if (or (probe-file fqn)
	    (probe-file (append-filename-extension fqn ".lisp")))
	(progn
	  (format t "Loading configuration file ~A~%" fqn)
	  (load fqn))
      (cyco-warning
       (sformat "Configuration file ~S does not exists." fqn)))))

(defun load-config ()
  "Loads the primary configuration file."
  (load-config-file *cyco-config-file*))

