;;;; CYCO Plugins
;;;;

(setf *config-directory* (join-path (user-home) ".config/cyco" :as-file))
(setf *plugin-directory* (join-path *config-directory* "plugins" :as-file))
(setf *current-plugin* nil)


(let ((registry '()))

  (defun load-plugin-file (filename)
    "Load lisp file relative to current plugin directory."
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
    "Clear plugin registry.
Note forget-plugin does not un-define active plugin definitions, all
it does is clear the plugin registry."
    (setf registry '()))
  
  (defun plugin (name &optional reload)
    "Load named plugin.
Loaded plugins are not reloaded unless optional reload is true."
    (if (or reload (not (member name registry)))
	(progn
	  (push name registry)
	  (setf name (string-downcase name))
	  (let ((main-file (str+ name "-main")))
	    (setf *current-plugin* name)
	    (load-plugin-file main-file)))
      name))
  
  (defun sub-plugin (name)
    "Load a plugin as a 'sub-plugin' of the current plugin."
    (let ((temp *current-plugin*))
      (plugin name)
      (setf *current-plugin* temp)))

  (defun ?plugins ()
    "Display list of loaded and available plugins."
    (format t "Plugins  '+' indicates plugin has been loaded.~%")
    (dolist (fqn (directory (sformat "~a/*" *plugin-directory*)))
      (let* ((nstr (namestring fqn))
	     (plugin-name (->symbol (second (split-path (subseq nstr 0 (1- (length nstr))))))))
	(format t "~A " (if (member plugin-name registry) "+" " "))
	(format t "~A~%" plugin-name)))) )

    
  
