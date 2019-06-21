;;;; CYCO
;;;;

(defun ?d (sym)
  "A shortcut abbreviation for describe"
  (describe sym))
  
(defun ?a (sym &optional (package :CYCO))
  "A shortcut abbreviation for apropos"
  (apropos sym package))

(defmethod ? ((n integer))
  (format t "INTEGER~%"))

(let ((ary (->vector (copies 16 '()))))
  (labels ((walk (inst)
		 (let* ((ci (channel-index inst))
			(ilst (aref ary ci)))
		   (push (name inst) ilst)
		   (setf (aref ary ci) ilst)
		   (dolist (c (children inst))
		     (walk c)))))
    (defun ?channels ()
      "Displays list of instruments on each MIDI channel."
      (setf ary (->vector (copies 16 '())))
      (walk *root-instrument*)
      (dotimes (i (length ary))
	(let ((chan (1+ i))
	      (ilst (aref ary i)))
	  (dolist (iname ilst)
	    (format t "[~2D] ~A~%" chan iname)))))))

(defun ?o (&optional (root *root-instrument*))
  "Displays orcestra tree."
  (print-tree root))

(defun ?p ()
  "Displays project structure."
  (if *project*
      (print-tree *project*)
    (format t "There is no current project, *PROJECT* is NIL~%")))

(defun ?? ()
  "Displays list of available inspection functions."
  (format t "(? object)   Displays info about object.~%")
  (format t "(?a symbol)  Shortcut for (apropos symbal)~%")
  (format t "(?d symbol)  Shortcut for (describe symbol)~%")
  (format t "(?o)         Displays orchestra tree.~%")
  (format t "(?p)         Displays current project structure.~%"))

