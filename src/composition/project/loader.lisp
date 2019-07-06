;;;; CYCO3 composition/project/loader
;;;;

(let ((current-project-main-file nil)
      (current-filename nil)
      (frmt "Loading project file ~A~%")
      
      (load-project-docstring
        "Loads the main project file.

name should be a symbol matching the project's directory.

The optional project-directory and main-file arguments are used to 
specify a non-standard location.

See LP as a more convenient method of loading a project.")
      
      (lp-docstring
         "LP is a short-cut for load-project with useful defaults.
The first time it is used the optional name argument should be specified as in 
load-project.  Thereafter subsequent calls to LP will default to reloading the 
current project.  

Whenever a new project is loaded it's name is saved to a file in the configuration
directory.  The next time CYCO is ran the previous project may be reloaded simply 
by calling LP without an argument.")
      
      (load-project-file-docstring
         "load-project-file loads a lisp file relative to the project's directory.
The name argument is a symbol which is converted to a lowercase string.
See LPF for a convenient shortcut.")
      
      (lpf-docstring
         "LPF is a short-cut for load-project-file.  When used without an argument
it reloads the most recent project-file."))
      
  (flet ((default-project-name (name)
	   (string-downcase
	    (->string (or name
			  (and (project-p *project*)(name *project*))
			  (read-persistent-project-name))))))
  
    (defun load-project (name &key
			      (project-directory *default-project-directory*)
			      main-file)
      load-project-docstring
      (let ((project-name (default-project-name name)))
	(if project-name
	    (let ((fqn (join-path project-directory project-name
				  (or main-file
				      (format-project-main-filename project-name))
				  :as-file)))
	      (setf current-project-main-file fqn)
	      (format t frmt fqn)
	      (load fqn))
	  (cyco-composition-error
	   'load-project
	   "Either there is no default project name,"
	   (sformat "or name argument is invalid: ~A ~A" (type-of name) name)))))
    
    (defun lp (&optional name)
      lp-docstring
      (load-project name))
    
    (defun load-project-file (name)
      load-project-file-docstring
      (let ((project-file-name (cond ((symbolp name)
				      (string-downcase (->string name)))
				     ((stringp name)
				      name)
				     (t (cyco-type-error 'load-project-file '(string symbol) name)))))
	(if project-file-name
	    (let* ((project-path (path-parent current-project-main-file))
		   (fqn (join-path project-path project-file-name :as-file)))
	      (setf current-filename fqn)
	      (format t frmt fqn)
	      (load fqn)))))
    
    (defun lpf (&optional name)
      lpf-docstring
      (if name
	  (load-project-file name)
	(if current-filename
	    (progn 
	      (format t frmt current-filename)
	      (load current-filename))
	  (cyco-composition-error 'lpf name
				  "Section does not exists")))) ))
  
  