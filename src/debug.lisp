;;;; cyco3 debug.lisp
;;;;
;;;; Functions for interactive debugging.


;; List of source files to reload.
;; If empty all cyco sources files are loaded.
;;
(param *rebuild-manifest* '(
			    "src/debug"
			    "src/generics"
			    "src/parts/qball-transform"
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
	  
