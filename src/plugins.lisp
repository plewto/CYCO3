;;;; CYCO Plugins
;;;;

(setf *config-directory* (join-path (user-home) ".config/cyco" :as-file))
(setf *plugin-directory* (join-path *config-directory* "plugins" :as-file))
(setf *current-plugin* nil)


(let ((registry '()))

  (defun load-plugin-file (filename)
    (let ((fqn (join-path *plugin-directory*
			  *current-plugin*
			  filename :as-file)))
      (if (or (probe-file fqn)
	      (probe-file (append-filename-extension fqn ".lisp")))
	  (progn
	    (format t "Loading plugin file: ~A~%" fqn)
	    (load fqn)
	    t)
	(cyco-warning
	 (sformat "Plugin file ~S does not exists." fqn)))))

  (defun forget-plugins ()
    (setf registry '()))
  
  (defun plugin (name &optional reload)
    (if (or reload (not (member name registry)))
	(progn
	  (push name registry)
	  (setf name (string-downcase name))
	  (let ((main-file (str+ name "-main")))
	    (setf *current-plugin* name)
	    (load-plugin-file main-file)))
      name))
  
  (defun sub-plugin (name)
    (let ((temp *current-plugin*))
      (plugin name)
      (setf *current-plugin* temp))) )
    
  
