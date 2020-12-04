;;;; CYCO composition/project framework.lisp
;;;;
;;;; Defines convenience functions to create empty project.
;;;;

(in-package :cyco)

(defun ?projects ()
  "Displays contents of *PROJECTS-ROOT*"
  (format t "Contents of project directory ~A~%" *projects-root*)
  (format t "*PROJECT-MAIN-FILENAME-FORMAT* is ~A~%" *project-main-filename-format*)
  (format t "*DEFAULT-PROJECT-OUTPUT-DIRECTORY* is ~A~%" *default-project-output-directory*)
  (dolist (fqn (directory (sformat "~a/*" *projects-root*)))
    (let* ((nstr (namestring fqn))
	   (pname (second (split-path (subseq nstr 0 (1- (length nstr)))))))
      (format t "  ~A~%" pname))))
	   

(flet ((format-name (sym)
		    (string-downcase (->string sym))))

  (defun create-project-framework (files)
    "Creates empty project directory.
The files argument specifies the project name and any additional empty files 
to be created.   

(create-project-framework 'foo)
    Create project directory foo with main file and MIDI directory.

(create-project-framework '(foo bar baz))
    As above but also creates empty files bar.lisp and baz.lisp"

    (let* ((flist (->list files))
	   (project-name (car flist))
	   (extras (cdr flist))
	   (pname (format-name project-name))
	   (main-file (sformat *project-main-filename-format* project-name))
	   (pdir (join-path *projects-root* pname))
	   (outdir (join-path pdir *default-project-output-directory*)))
      (format t "Creating project ~A directory framework~%" project-name)
      (dolist (d (list pdir outdir))
	(format t "Ensuring directory exist: ~A~%" d)
	(ensure-directories-exist d))
      (dolist (fn (cons main-file (->list extras)))
	(let ((fqn (append-filename-extension
		    (join-path pdir (format-name fn) :as-file)
		    ".lisp")))
	  (if (not (probe-file fqn))
	      (progn
		(format t "Creating project file: ~A~%" fqn)
		(let ((stream (open fqn
				    :direction :output
				    :if-does-not-exist :create)))
		  (format stream ";;;; CYCO Project ~A  File ~A.lisp~%" pname (format-name fn))
		  (format stream ";;;;~%~%")
		  (if (string= fn main-file)
		      (progn
		  	(format stream (sformat "(version ~A)~%~%" (car +cyco-version+)))
		  	(format stream (sformat "(project ~A)~%" project-name))))
		  (close stream)))
  	    (format t "Project file already exists: ~A~%" fqn)))))))
