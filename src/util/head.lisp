;;;; CYCO3 util head
;;;;
;;;; Utility to print first few line of a file.
;;;;

(labels ((comment-line-p (line)
			 (setf line (string-left-trim '(#\space #\tab #\newline) line))
			 (and (plusp (length line))
			      (char= (char line 0) #\;)))

	 (more-p (line count)
		     (and line
			  (if (or (not count)(eq count :comments))
			      (comment-line-p line)
			    (and (numberp count)(plusp count)))))

	 (find-main-files ()
			  (remove-if-not #'(lambda (s)(numberp (search "main" (string-downcase s))))
					 (directory-contents :exclude '(:h :d) :form :short)))

	 (resolve-filenames (base)
			    (cond ((eq base 'main)
				   (find-main-files))
				  ((symbolp base)
				   (let ((fn (str+ (string-downcase (->string base)) ".lisp")))
				     (->list (join-path (cwd) fn :as-file))))
				   (t
				    (->list (resolve-special-directory base)))))

	 (print-banner (filename enable)
		       (if enable
			   (progn 
			     (format t "~%***********************************~%")
			     (format t "*** ~S~%" filename)
			     (format t "***~%~%"))
			 (format t "~%")))

	 (head-1 (filename count)
		 (let ((lines (with-open-file (stream filename)
					      (loop for line = (read-line stream nil)
						    while (prog1 (more-p line count)
							    (if (numberp count)
								(setf count (1- count))))
						    collect line))))
		   (dolist (line lines)
		     (format t "~A~%" line))
		   (format t "~%"))) )

	(defun head (filename &key (count :comments)(banner t))
	  (dolist (fn (resolve-filenames filename))
	    (print-banner fn banner)
	    (head-1 fn count))) )

;; Docstrings

(setf (documentation 'head 'function)
      "Prints the first few lines of a file.

filename - File name to process.
           If the the filename is the symbol 'main, all files in the CWD which 
           contain 'main' as a substring are processed.
           If the filename is some other symbol, it is taken as a lisp file in the CWD.
           The symbol name is converted to lower-case.

:count   - Number of lines to print.
           If the set to :comments, then all initial lines whose first non-space character
           is the Lisp comment ; are displayed.  Defaults to :comments.

:banner  - Bool, if true print filename banner, default t.


Head may easily be used to print the initial comments of the current project's main file.

(head 'main)")
