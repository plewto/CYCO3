;;;; Steel Banks Common Lisp specific
;;;;

(format t "Using SBCL options~%")

(defun set-cyco-prompt ()
  (setf sb-int:*REPL-PROMPT-FUN*
	#'(lambda (s)
	    (if (project-p *project*)
		(format s "~%CYCO(~A.~A): "
			(name *project*)
			(name (property *project* :current-section)))
	      (format s "~%CYCO: ")))))

(defun save-snapshot (filename)
  "Save snapshot of CYCO as an executable."
  (let ((fname (resolve-user-home filename)))
    (format t "Creating CYCO executable, ~S~%" fname)
    (setf sb-int:*REPL-PROMPT-FUN* #'(lambda (s)(format s "~%* ")))
    (sb-ext::save-lisp-and-die fname :executable t)))

(defun exit ()
  (sb-ext:exit))

