;;;; CYCO composition/project framework.lisp
;;;;
;;;; Defines convenience functions to create empty project.
;;;;

(in-package :cyco)

(defun ?projects (&optional (pat "")(print t))
  (let ((count 0)
	(acc '()))
    (dolist (fqn (directory (sformat "~A/*" *projects-root*)))
      (let* ((nstr (namestring fqn))
	     (pname (second (split-path (subseq nstr 0 (1- (length nstr)))))))
	(if (search pat pname)
	    (push (cons count pname) acc))
	(setf count (1+ count))))
    (setf acc (reverse acc))
    (if print
	(prog1
	     (format t "Contents of project directory ~A~%" *projects-root*)
	  (format t "*PROJECT-MAIN-FILENAME-FORMAT* is ~A~%" *project-main-filename-format*)
	  (format t "*DEFAULT-PROJECT-OUTPUT-DIRECTORY* is ~A~%" *default-project-output-directory*)
	  (dolist (item acc)
	    (format t "[~3D] ~A~%" (car item)(cdr item))))
      acc)))

	   

(flet ((format-name (sym)
		    (string-downcase (->string sym))))

  (defun create-project-framework (files)
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


;; Docstrings


(setf (documentation '?projects 'function)
      "Displays contents of *PROJECTS-ROOT*
Each item is preceded by an index number.
Please note, --ALL-- sub-directories under *PROJECTS-ROOTS* are included 
regardless if they are actual project folders or not.

If optional pat argument is non-empty string, include only directoies which 
include pat as a sub-string.

If optional print argument is nil, returns list of directories instead of 
printing them. 

The project number may be used as an argument to LOAD-PROJECT and LP.")


(setf (documentation 'create-project-framework 'function)
         "Creates empty project directory.
The files argument specifies the project name and any additional empty files 
to be created.   

(create-project-framework 'foo)
    Create project directory foo with main file and MIDI directory.

(create-project-framework '(foo bar baz))
    As above but also creates empty files bar.lisp and baz.lisp")
