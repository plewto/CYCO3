;;;; CYCO local-config
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
  (load-config-file *cyco-config-file*))
