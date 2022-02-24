;;;; CYCO composition/project loader.lisp
;;;;
;;;; Functions for loading project files.
;;;;

(in-package :cyco)


(global *current-lpf-filename* nil)  ;; ISSUE: Dont like this as a global.

(let ((current-project-main-file nil)
      (frmt "Loading project file ~A~%")
      
      (load-project-docstring
        "Loads the main project file.

name should be a symbol matching the project's directory.

The optional project-directory and main-file arguments are used to 
specify a non-standard location.

The CWD is set to the projects location. 

See LP as a more convenient method of loading a project.")
      
      (lp-docstring
         "LP is a short-cut for load-project with useful defaults.
The first time it is used the optional name argument should be specified as in 
load-project.  Thereafter subsequent calls to LP will default to reloading the 
current project.  

Whenever a new project is loaded its name is saved to a file in the configuration
directory.  The next time CYCO is ran the previous project may be reloaded simply 
by calling LP without an argument.")
      
      (load-project-file-docstring
         "load-project-file loads a lisp file relative to the project's directory.
The name argument is a symbol which is converted to a lowercase string.
See LPF for a convenient shortcut.")
      
     (lpf-docstring
         "LPF is a short-cut for load-project-file.  When used without an argument
it reloads the most recent project-file.  Do not quote the argument."))

      
  (flet ((default-project-name (name)
	   (string-downcase
	    (->string (or name
			  (and (project-p *project*)(name *project*))
			  (read-persistent-project-name))))))
  
    (defun load-project (name &key
			      (project-directory *projects-root*)
			      main-file)
      load-project-docstring
      (let ((project-name (default-project-name name)))
	(if project-name
	    (let ((fqn (join-path project-directory project-name
				  (or main-file
				      (format-project-main-filename project-name))
				  :as-file)))
	      (setf current-project-main-file fqn)
	      (if *enable-banners*
		  (format t frmt fqn))
	      (load fqn)
	      (cwd (car (split-path fqn)))
	      t)
	  (cyco-composition-error
	   'load-project
	   "Either there is no default project name,"
	   (sformat "or name argument is invalid: ~A ~A" (type-of name) name)))))
    
    (defmacro lp (&optional name)
      `(if ',name
	   (load-project ',name)
	 (load-project nil)))

    (setf (documentation 'lp 'function)
	  lp-docstring)
    
    (defun load-project-file (name)
      load-project-file-docstring
      (let ((project-file-name
	     (cond ((symbolp name)
		    (string-downcase (->string name)))
		   ((stringp name)
		    name)
		   (t (cyco-type-error 'load-project-file '(string symbol) name)))))
	(if project-file-name
	    (let* ((project-path (path-parent current-project-main-file))
		   (fqn (join-path project-path project-file-name :as-file)))
	      (if *enable-banners*
		  (format t frmt fqn))
	      (load fqn)))))
  
    (defmacro lpf (&optional name)
      `(if ',name
	   (progn
	     (setf *current-lpf-filename* ',name)
	     (load-project-file ',name))
	 (if *current-lpf-filename*
	     (progn 
	       (format t "Reloading ~A~%" *current-lpf-filename*)
	       (load-project-file *current-lpf-filename*))
	   (cyco-composition-error 'lpf ',name "Section does not exixts."))))


    (setf (documentation 'lpf 'function) lpf-docstring)))
