;;;; CYCO3 composition/project/persistence
;;;;
;;;; The project 'persistence' feature saves the current project name to
;;;; the configuration directory.  Upon the next application run the
;;;; previous project may be reloaded by entering (lp) without 
;;;; specifying a project name.
;;;;

(global *persistent-project-name-namestring* "current-project-name")


(defun save-persistent-project-name (project-name)
  "Saves the current project name to a configuration file."
  (let* ((filename (join-path *config-directory*
			      *persistent-project-name-namestring*
			      :as-file))
	 (stream (open filename
		       :direction :output
		       :if-does-not-exist :create
		       :if-exists :overwrite)))
    (format t "BUG 0008 (SAVE-PERSISTENT-PROJECT-NAME ~S~%" PROJECT-NAME) ;; TODO remove test
    (format stream (->string project-name))
    (close stream)))

(defun read-persistent-project-name ()
"Returns the previous project name from a configuration file.
If the configuration file does not exists, returns nil."
  (let* ((filename (join-path *config-directory*
			      *persistent-project-name-namestring*
			      :as-file))
	 (stream (if (probe-file filename)
		     (open filename
			   :direction :input))))
    (if stream
	(prog1
	    (read-line stream)
	  (close stream))
      nil))) 
