;;;; CYCO Inspection
;;;;

(defun ?d (sym)(describe sym))
(defun ?a (sym)(apropos sym))

(defmethod ? ((n integer))
  (format t "INTEGER~%"))

(defun ?tree (&optional (what 'o))
  (let ((options '(o p)))
    (cond ((eq what 'p)   			; project
	   (if (project-p *project*)
	       (print-tree *project*)
	     (format t "There is no current project.~%")))
	  ((eq what 'o)			        ; orchesgtra
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
      (setf ary (->vector (copies 16 '())))
      (walk +root-instrument+)
      (dotimes (i (length ary))
	(let ((chan (1+ i))
	      (ilst (aref ary i)))
	  (dolist (iname ilst)
	    (format t "[~2D] ~A~%" chan iname)))))))
		       
  
  
