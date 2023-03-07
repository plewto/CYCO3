;;;; cyco3 debug.lisp
;;;;
;;;; Functions for interactive debugging.


;; List of source files to reload.
;; If empty all cyco sources files are loaded.
;;
(param *rebuild-manifest* '(
			    "src/debug"
			    "src/util/head"
			    "src/util/shell-utilities"
			    ))



;; Hook function executed after rebuild.
;;
(defun rebuild-hook () )
 


;; Reloads cyco source file.
;;
(defun rebuild ()
  (cwd "@cyco")
  (if (not *rebuild-manifest*)
      (build-cyco)
    (dolist (file *rebuild-manifest*)
      (format t "Loading ~A~%" file)
      (load file)))
  (rebuild-hook))


(defun rb ()(rebuild))
(defun x ()(exit))

;; Insert in code under test to trace execution.
;;
(defun db-marker (n &rest args)
  (format t "*** DEBUG MARKER ~A : " n)
  (dolist (a args)
    (format t "~A " a))
  (format t " ***~%"))
	  


(let ((depth 0)
      (enable t))

  (labels ((pad ()(scopies (* 4 depth) " "))
	   (pad- ()(scopies (* 4 (max 0 (1- depth))) " "))
	   (depth+ ()(setf depth (1+ depth)))
	   (depth- ()(setf depth (max 0 (1- depth)))))

	  (defun trace-enable (flag)
	    (setf enable flag))

	  (defun trace-enter (text)
	    (if enable
		(progn
		  (format t "TRACE [~2D]~A --> ~A~%" depth (pad) text)
		  (depth+))))

	  (defun trace-exit (&optional (text ""))
	    (if enable
		(progn
		  (depth-)
		  (format t "TRACE [~2D]~A <-- ~A~%" depth (pad) text))))

	  (defun trace-marker (text)
	    (if enable
		(format t "TRACE [~2D]~A --- ~A~%" (1- depth)(pad-) text))) ))

		  
