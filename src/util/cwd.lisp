;;;; CYCO3 util/cwd.lisp
;;;;
;;;; Defines functions to list directory contents.
;;;;

(let ((special-directories '()))

  (labels ((keys ()
		 (mapcar #'car special-directories))

	   (translate (key)
		      (let ((entry (assoc key special-directories :test #'string=)))
			(and entry
			     (resolve-user-home (funcall (third entry)))))) )

	  (defun push-special-directory (key remarks function)
	    (push (list key remarks function) special-directories)
	    (keys))


	  (defun pop-special-directory (key &key from-end)
	    (let ((entry (find key special-directories
			       :from-end from-end
			       :test #'(lambda (a b)(string= a (car b))))))
	      (and entry
		   (setf special-directories (delete entry special-directories)))
	      (keys)))
	  

	  (defun resolve-special-directory (key)
	    (or (translate key) key))
	  

	  (defun ?special-directories ()
	    (dolist (entry special-directories)
	      (let* ((key (car entry))
		     (location (resolve-special-directory key))
		     (remarks (second entry)))
		(format t "~12A -> ~20A   location: ~S~%" key remarks location)))
	    nil)
			
	  
	  (push-special-directory *os-homedir-alias* "User home" #'user-home)
	  (push-special-directory "." "Current directory" #'(lambda () (namestring *default-pathname-defaults*)))
	  (push-special-directory ".." "Parent directory"
				  #'(lambda ()
				      (let ((current (namestring *default-pathname-defaults*)))
					(if (and (stringp current)
						 (plusp (length current)))
					    (str+
					     (car (split-path (subseq current 0 (1- (length current)))))
					     *OS-PATH-SEPARATOR*)))))
	  
	  (push-special-directory "@config" "CYCO configuration" #'(lambda () *config-directory*))
	  (push-special-directory "@cyco" "CYCO3 folder" #'(lambda () *cyco-location*))
	  (push-special-directory "@projects" "CYCO projects" #'(lambda () *projects-root*))

	  (defun cwd (&optional dir)
	    (let ((new-dir (if dir
			       (let* ((parts (partition-path dir))
				      (head (car parts))
				      (tail (cdr parts)))
				 (if (string= (->string *os-path-separator*)(subseq dir 0 1))
				     (progn 
				       (setf tail (cons head tail))
				       (setf head "")))
				 (setf head (resolve-special-directory head))
				 (join-path-list (cons head tail))))))
	      (if new-dir
		  (setf *default-pathname-defaults* (truename new-dir))))
	    (namestring *default-pathname-defaults*)) ))


;; Docstrings

(setf (documentation 'push-special-directory 'function)
      "Adds a new CWD directory alias.
If a directory string passed to CWD begins with the string key, 
key is replaced the result of calling the function.

key - string
remarks - descriptive text
function - (lambda () ) -> string, returns replacement text for key.

If a replacement for key has already been set, the new value shadows the
previous one.

Returns list of assigned keys.")


(setf (documentation 'pop-special-directory 'function)
      "Removes a special-directory assignment for key.
If there are more then one assignment for key, the previous copy
comes into effect.

Returns list of assigned keys.")


(setf (documentation 'resolve-special-directory 'function)
      "Replaces key with it's assigned special-directory value.
If key does not signify a special-directory, returns key.")


(setf (documentation '?special-directories 'function)
      "Prints list of CWD special-directory assignments.")

(setf (documentation 'cwd 'function)
      "Returns, and optionally changes, the Current Working Directory.
If the directory is being changed and the first element of the path is
a special-directory, the first path element is replaced with the special 
directory.

Use (?SPECIAL-DIRECTORIES) for a list of directory assignments.")
