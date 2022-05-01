;;;; CYCO plugins.lisp
;;;;
;;;; Plugins provide additional features and configuration.
;;;; Plugins may be located in multiple locations but the following
;;;; two are expected:
;;;;
;;;;    cyco/plugins/             ;; Plugins provided with CYCO
;;;;    ~/.config/cyco/plugins/   ;; User defined plugins
;;;;
;;;; Additional locations may be specified by calling
;;;;
;;;;       (PUSH-PLUGIN-SEARCH-PATH directory-name)
;;;;
;;;; If more then one directory contains the same plugin, the location
;;;; added to the search-path last is used.   By default plugins 
;;;; in ~/.config/cyco/plugins/ shadow those in cyco/plugins/
;;;; 

(in-package :cyco)

(let ((search-path '())
      (current-plugin-stack '())
      (registry '()))
    (labels ((format-name
	      (name)
	      (string-downcase (join-path name (sformat "~A-main.lisp" name) :as-file)))
	     
	     (find-plugin-location
	      (main-name path-list &optional verbose)
	      (if (not path-list)
		  nil
	      (let* ((fqn (join-path (car path-list) main-name :as-file))
		     (found (probe-file fqn)))
		(if verbose
		    (format t "Probing plugin directory ~S   found ~A~%" fqn (bool found)))
		(or (and found fqn)
		    (find-plugin-location main-name (cdr path-list) verbose))))))
  
      (defun push-plugin-search-path (directory)
	"Add new directory to plugin search-path.

Later additions shadow earlier ones."
	(let ((key (sformat "@plugin-~D" (length search-path))))
	  (prog1
	      (push directory search-path)
	    (push-special-directory key "Plugin directory" #'(lambda () (namestring directory))))))

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
      
      (defun find-plugin (name &optional verbose)
	"Locates main file for named plugin.
A nil result indicates a matching plugin can not be found."
	(find-plugin-location (format-name name) search-path verbose))

      (defun current-plugin-path ()
	(string-downcase (car current-plugin-stack)))
      
      (defun load-plugin-file (name)
	"Loads a file relative to the current plugin."
	(let ((fqn (string-downcase (join-path (car current-plugin-stack) name :as-file))))
	  (setf fqn (append-filename-extension fqn ".lisp"))
	  (if (probe-file fqn)
	      (progn 
		(format t "Loading plugin file: ~A~%" fqn)
		(load fqn))
	    (cyco-error
	     (sformat "Plugin file ~S does not exists" fqn)))))

      (defun external-load-plugin-file (name plugin)
	"Loads file relative to plugin directory.

name - file name relative to plugin directory, may be string or symbol
       Adding .lisp extension is not necessary.
plugin - plugin name, may be string or symbol.

While this function allows a plugin file to be loaded from anywhere, 
that's probably not the best practice.   It's real intent is to allow 
a plugin to load optional files after the plugin itself has been loaded.
The LOAD-PLUGIN-FILE function is only usable while the main plugin file
is being loaded.   

A use case is a plugin which defines a bunch of synth drum kits.
Collectively these tend to define a lot of static data.
external-load-plugin-file may be used to load only those specific kits
actually being used."
       
	(let* ((directory (find-plugin-location (string-downcase (->string plugin)) search-path))
	       (fqn (and directory (join-path directory (string-downcase (->string name)) :as-file))))
	  (if fqn
	      (progn
		(setf fqn (append-filename-extension fqn ".lisp"))
		(if (probe-file fqn)
		    (progn
		      (format t "Loading plugin file: ~A~%" fqn)
		      (load fqn)
		      fqn)
		  (cyco-error
		   (sformat "Plugin file ~S does not exists" fqn))))
	    (cyco-error (sformat "Can not load external ~A plugin file: ~A" plugin name)))))
	    
      
      (defun load-plugin (name &key reload verbose)
	"Loads the main plugin file.  

name     - quoted symbol
:reload  - Boolean, if true the plugin is reloaded whether it has been
           previously loaded or not.
:verbose - If true display main-file search attempts. 

See PLUGIN macro"
	(if (or reload (not (member name registry)))
	     (let ((main (find-plugin name verbose)))
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
	`(load-plugin ',name :reload ,reload :verbose nil))

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

