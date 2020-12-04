;;;; CYCO plugins.lisp
;;;;
;;;; Plugins provide additional features and configuration.
;;;; Plugins may be located in multiple locations but the following
;;;; two are expected:
;;;;
;;;;    cyco3/plugins/            ;; Plugins provided with CYCO
;;;;    ~/.config/cyco/plugins/   ;; User defined plugins
;;;;
;;;; Additional locations may be specified by calling
;;;;
;;;;       (PUSH-PLUGIN-SEARCH-PATH directory-name)
;;;;
;;;; If more then one directory contains the same plugin, the location
;;;; added to the search-path last is used.   By default plugins 
;;;; in ~/.config/cyco/plugins/ shadow those in cyco3/plugins/
;;;; 

(in-package :cyco)

(let ((search-path '())
      (current-plugin-stack '())
      (registry '()))
    (labels ((format-name
	      (name)
	      (string-downcase (join-path name (sformat "~A-main.lisp" name) :as-file)))
	     
	     (find-plugin-location
	      (main-name path-list)
	      (if (not path-list)
		  nil
	      (let* ((fqn (join-path (car path-list) main-name :as-file))
		     (found (probe-file fqn)))
		(or (and found fqn)
		    (find-plugin-location main-name (cdr path-list)))))))
  
      (defun push-plugin-search-path (directory)
	"Add new directory to plugin search-path.

Later additions shadow earlier ones."
	(push directory search-path))

      (defun forget-plugins ()
	"Marks all plugins as 'unloaded'

Normally if a plugin has been loaded, additional attempts to reload it are
ignored.  Calling forget-plugins does not actually remove any plugin from
memory.  Instead it simply marks them as never having been loaded."
	(setf registry '()))
      
      (defun ?plugin-search-path ()
	"Prints list of plugin search paths."
	(format t "Plugin search path:~%")
	(dolist (item search-path)
	  (format t "    ~A~%" item)))
      
      (defun find-plugin (name)
	"Locates directory for named plugin.
A nil result indicates a matching plugin can not be found."
	(find-plugin-location (format-name name) search-path))
      
      (defun load-plugin-file (name)
	"Loads a file relative to the current plugin."
	(let ((fqn (string-downcase (join-path (car current-plugin-stack) name :as-file))))
	  (setf fqn (append-filename-extension fqn ".lisp"))
	  (if (probe-file fqn)
	      (progn 
		(format t "Loading plugin file: ~A~%" fqn)
		(load fqn))
	    (cyco-error
	     (sformat "Plugin file ~A does not exists" fqn)))))
      
      (defun load-plugin (name &optional reload)
	"Loads the main plugin file.  

name - quoted symbol
reload - Boolean, if true the plugin is reloaded whether it has been
previously loaded or not. 

See PLUGIN macro"
	(if (or reload (not (member name registry)))
	     (let ((main (find-plugin name)))
	       (if main
		   (progn
		     (format t "Loading plugin ~A~%" main)
		     (push (path-parent main) current-plugin-stack)
		     (load main)
		     (pop current-plugin-stack)
		     (push name registry))
		 (cyco-error
		  (sformat "Could not find plugin: ~A" name))))))
      
      (defmacro plugin (name &optional reload)
	"Loads named plugin

PLUGIN is a convenience macro for LOAD-PLUGIN.   With plugin there is 
no need to quote the name argument."
	`(load-plugin ',name ,reload))

      (defun ?plugins ()
	"List all plugins
An asterisk at the beginning of a line indicates that plugin has been loaded."
	(format t "Plugins:~%")
	(dolist (spath search-path)
	  (format t "Plugin directory: ~A~%" spath)
	  (dolist (fqn (directory (sformat "~a/*" spath)))
	    (let* ((nstr (namestring fqn))
		   (plugin-name (->symbol (second (split-path (subseq nstr 0 (1- (length nstr))))))))
	      (format t "    ~A " (if (member plugin-name registry) "*" " "))
	      (format t "~A~%" plugin-name))))
	  nil) ))
