;;;; CYCO3 src/inspection
;;;;

(defun ?d (sym)
  "A shortcut abbreviation for describe"
  (describe sym))
  
(defun ?a (sym &optional (package :CYCO))
  "A shortcut abbreviation for apropos"
  (apropos sym package))

(defmethod ? ((n integer))
  (format t "INTEGER~%"))

(defun ?tree (&optional (what 'o))
  "Displays tree structure.
Optional what argument indicates which tree to display
'o - show orchestra tree starting with +root-instrument+
'p - show project tree using *project*"
  (let ((options '(o p)))
    (cond ((eq what 'p)   			; project
	   (if (project-p *project*)
	       (print-tree *project*)
	     (format t "There is no current project.~%")))
	  ((eq what 'o)			        ; orchestra
	   (print-tree +root-instrument+))
	  (t "Expected optional argument on of ~A~%" options))))

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
      (walk +root-instrument+)
      (dotimes (i (length ary))
	(let ((chan (1+ i))
	      (ilst (aref ary i)))
	  (dolist (iname ilst)
	    (format t "[~2D] ~A~%" chan iname)))))))
