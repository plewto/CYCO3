;;;; cyco3 util/dirlist.lisp
;;;;
;;;; Functions for listing directory contents.
;;;;

(labels ((format-item (item)
		      (let ((acc "")
			    (att (car item))
			    (name (second item)))
			(if (member :D att)
			    (setf acc "D")
			  (setf acc "F"))
			(if (member :L att)
			    (setf acc (str+ acc "L"))
			  (setf acc (str+ acc " ")))
			(if (member :H att)
			    (setf acc (str+ acc "H"))
			  (setf acc (str+ acc " ")))
			(sformat "~A : ~S" acc name)))
			
	 (sort-predicate (a b)
			 (let ((a-tail (second (split-path (second a))))
			       (b-tail (second (split-path (second b)))))
			   (string> a-tail b-tail)))
			       
	 
	 (sort-content-list (contents)
			    (let ((dcc '())
				  (rcc '()))
			      (dolist (item contents)
			      	(if (member :D (car item))
			      	    (push item dcc)
			      	  (push item rcc)))
			      (append dcc rcc)))
	 
	 (is-directory-p (namestring)
			   (and (plusp (length namestring))
				(string= (subseq (reverse namestring) 0 1)
					 (->string *os-path-separator*))))
	 
	 (is-file-p (namestring)
		    (not (is-directory-p namestring)))

	 (is-hidden-p (namestring)
		      (if (is-directory-p namestring)
			  (setf namestring (subseq namestring 0 (1- (length namestring)))))
		      (let ((name (final (split-path namestring))))
			(and (plusp (length name))
			     (string= (subseq name 0 1) "."))))
	 
	 (is-link-p (namestring)
		    (declare (ignore namestring))
		    nil)
	 
	 (exclude-p (item flags)
		   (let ((att (car item))
			 (rs nil))
		     (dolist (f (->list flags))
		       (if (member f att)
			   (setf rs t)))
		     rs))

	 (match-p (item pat invert)
		  (if pat
		      (if invert
			  (not (search pat (second item)))
			(search pat (second item)))
		    t)) )

	(defun directory-contents (&key (dir nil)(exclude nil)(match nil)(invert nil))
	  (let ((temp-dir nil))
	    (if dir
		(progn
		  (setf temp-dir (cwd))
		  (setf dir (cwd dir)))
	      (setf dir (cwd)))
	    (let ((contents (delete-duplicates
			     (append (directory (join-path dir "*" :as-file) :resolve-symlinks nil)
				     (directory (join-path dir "*.*" :as-file) :resolve-symlinks nil))))
		  (acc '()))
	      (if temp-dir (cwd temp-dir))
	      (dolist (pathspec contents)
		(let ((namestring (namestring pathspec))
		      (attributes '()))
		  (if (is-hidden-p namestring)
		      (push :h attributes))
		  (if (is-link-p namestring)
		      (push :l attributes))
		  (if (is-directory-p namestring)
		      (push :d attributes)
		    (push :f attributes))
		  (push (list attributes namestring) acc)))
	      (dolist (item (copy-list acc))
		(if (exclude-p item exclude)
		    (setf acc (delete item acc)))
		(if (not (match-p item match invert))
		    (setf acc (delete item acc))))
	      (sort-content-list acc))))

	(defun ls (&key (dir nil)(exclude '(:h))(match nil)(invert nil))
	  (format t "Lising for ~A~%" (or dir (cwd)))
	  (let ((contents (directory-contents :dir dir :exclude exclude :match match :invert invert)))
	    (dolist (item contents)
	      (format t "~A~%" (format-item item))))) )

;; Docstrings
;;

(setf (documentation 'directory-contents 'function)
      "Returns list of directory contents.

:dir - The directory, defaults to (CWD)

:exclude - list of file attributes to exclude.
    May be one of :H - hidden
                  :L - link (currently not supported)
                  :D - directory
                  :F - regular file
    :d and :f are mutually exclusive.
    The argument value may be a single keynumber or list of key numbers.


:match - string, Filter pattern.  Only filenames for which match is a sub-string
    are included.  

:invert - bool, reverse sens of :match

Returns a nested list where each element has the form 

         ((attributes) truename-of-file)

Attributes may contain any of the symbols :H :L :D and :F with the 
same meaning as the :match argument.")

(setf (documentation 'ls 'function)
      "Prints directory contents.
Arguments are identical to DIRECTORY-CONTENTS.
Returns nil")
