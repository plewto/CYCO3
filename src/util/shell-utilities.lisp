;;;; cyco3 util shell-utilities.lisp
;;;;
;;;; Defines a few shell commands callable from CYCO.

(global *shell-command* "/usr/bin/sh")

(defun ls (&optional dir (options ""))
  (let* ((path (resolve-special-directory (or dir (cwd))))
	 (cmd (list "-c" (sformat "ls ~A ~A~A" options path *os-path-separator*))))
    (with-output-to-string (s)
			   (sb-ext:run-program *shell-command* cmd :output s)
			   s)))


(labels ((build-options (exclude ignore)
			(let ((acc "")
			      (options '("-l"  ;; long format
					 "--indicator-style=slash" ;; Appends / to directory names
					 "--group-directories-first")))
			  (dolist (opt options)
			    (setf acc (sformat "~A ~A " acc opt)))
			  (if (not (member :h exclude))
			      (setf acc (str+ acc " -A ")))
			  (if ignore
			      (setf acc (sformat "~A --ignore=~A" acc ignore)))
			  acc))
	 
	 (string-starts-with (c s)
			     (and (stringp s)
				  (plusp (length s))
				  (char= c (char (string-downcase s) 0))))

	 (format-long (item) item)
	 (format-short (item)
		       (let ((pos (search ":" item)))
			 (if (numberp pos)
			     (subseq item (+ pos 4))
			   item)))
	 
	 (format-item (item form)
		      (cond ((eq form :long)
			     (format-long item))
			    ((eq form :short)
			     (format-short item))
			    (t (format-long item))))

	 (format-results (lst form)
			 (if (not lst)
			     nil
			   (cons (format-item (car lst) form)
				 (format-results (cdr lst) form)))))

	(defun directory-contents (&key dir (exclude :h) ignore (form :long))
	  (setf exclude (->list exclude))
	  (let* ((options (build-options exclude ignore))
		 (contents (split-string (ls dir options) (sformat "~%")))
		 (header (string-upcase (->string (car contents)))))
	    ;; Removes initial "total line if it exists.
	    (if (zerop (search "TOTAL" header))
		(setf contents (cdr contents)))
	    ;; Remove links
	    (if (member :l exclude)
		(setf contents (delete #\l contents :test #'string-starts-with)))
	    ;; Remove directories
	    (if (member :d exclude)
		(setf contents (delete #\d contents :test #'string-starts-with)))
	    ;; Remove regular files
	    (if (member :f exclude)
		(setf contents (delete #\- contents :test #'string-starts-with)))
	    (format-results contents form))) )


(labels ((ignore-p (item pattern invert)
		   (if (and pattern (plusp (length pattern)))
		       (let ((matched (search pattern item)))
		   	 (if invert
		   	     (not matched)
		   	   matched))
		     nil))

	
	 
	 (split-name-count (item)
			   (let* ((pos (search ":" item :from-end t)))
			     (if pos
				  (let ((name (subseq item 0 pos))
					(count (parse-integer (subseq item (1+ pos)))))
				    (cons name count))
			       (cons item 0)))) )
	 
	(defun grep (pattern &key (dir (cwd))(options "-ir")(ignore "")(invert nil)(print t))
	  (let* ((temp-dir (cwd)))
	    (cwd (resolve-special-directory dir))
	    (prog1
	    	(let* ((cmd (list "-c" (sformat "cd ~A; grep ~A ~A *" (cwd) options (sformat "~S" pattern))))
		       (raw-results (split-string
	    			    (with-output-to-string (s)
	    						   (sb-ext:run-program *shell-command* cmd :output s)
	    						   (->string s))(sformat "~%")))
	    	      (acc '()))
	    	  (dolist (item raw-results)
	    	    (if (not (ignore-p item (->string ignore) invert))
	    	  	(push item acc)))
		  (if print
		      (progn 
			(dolist (item acc)
			  (format t "~A~%" item))
			nil)
		    (reverse acc)))
	      (cwd temp-dir))))

	(defun grep-count (pattern &key dir (options "-ir")(ignore "")(invert nil)(print t))
	  (if (not (search "-c" options))
	      (setf options (sformat "~A -c " options)))
	  (if (not (search "--no-messages" options))
	      (setf options (sformat "~A --no-messages " options)))
	  (let ((raw-results (grep pattern :dir dir :options options :ignore ignore :invert invert :print nil))
		(acc '()))
	    (dolist (item raw-results)
	      (let ((name-count (split-name-count item)))
		(if (not (zerop (cdr name-count)))
		    (push (cons (cdr name-count)(car name-count)) acc))))
	    (setf acc (reverse acc))
	    (if print
		(progn
		  (dolist (item acc)
		    (format t "count ~3D : ~s~%" (car item)(cdr item)))
		  nil)
	      acc)))

	(defun grep2 (pat1 pat2 &key dir (options "-ir")(ignore "")(invert nil)(print t))
	  (if (not (search "-no-messages" options))
	      (setf options (sformat "~A --no-messages" options)))
	  (let* ((a (mapcar #'cdr (grep-count pat1 :dir dir :options options :ignore ignore :invert invert)))
		 (b (mapcar #'cdr (grep-count pat2 :dir dir :options options :ignore ignore :invert invert)))
		 (results (intersection a b :test #'string=)))
	    (if print
		(dolist (item results)
		  (format t "~A~%" item))
	      results))) )

;; Docstrings
;;

(setf (documentation 'ls 'function)
      "Call shell ls command

dir     - Optional directory, defaults to (CWD).
options - Flags passed to shell ls command.

Returns string")


(setf (documentation 'directory-contents 'function)
      "Returns list of directory contents.

:dir     - directory, default (CWD)

:exclude - List of file types to be excluded.   Possible values are
           :h - hidden (the default)
           :l - links
           :d - directories
           :f - regular files

           A single keyword may be used instead of a list.

:ignore - Shell pattern for names to be ignored.

:form   - Format may be either :short or :long (default)

Returns list of strings.")


(setf (documentation 'grep 'function)
      "Calls grep utility

pattern  - Regular expression search pattern.
:dir     - Directory, defaults to (CWD)
:options - String, command line flags passed to grep, default -ir
:ignore  - String, suppress all files which includes value as substring.
:invert  - Bool, invert sense of :ignore.
:print   - Bool, if true print results to terminal, default t

Returns list of :print is nil.")


(setf (documentation 'grep-count 'function)
      "Version of grep which returns count of pattern in each file.
Files with 0 count are ignored.   All arguments are identical to GREP.
Options -c and --no-messages are set and can not be changed.")

(setf (documentation 'grep2 'function)
      "Serial version, runs grep twice in sequence.
Returns only those files which contain both patterns pat1 and pat2.
With exception of the second pattern argument, usage is exactly the
same as grep.
Options -c and --no-messages are set and can not be changed.")


