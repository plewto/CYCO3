;;;; cyco3 debug.lisp
;;;;
;;;; Functions for interactive debugging.


;; List of source files to reload.
;; If empty all cyco sources files are loaded.
;;
(param *rebuild-manifest* '())


;; Hook function executed after rebuild.
;;
(defun rebuild-hook () )


;; Reloads cyco source file.
;;
(defun rebuild ()
  (if (not *rebuild-manifest*)
      (build-cyco)
    (dolist (file *rebuild-manifest*)
      (load file)))
  (rebuild-hook))


;; Insert in code under test to trace execution.
;;
(defun db-marker (n &rest args)
  (format t "*** DEBUG MARKER ~A : " n)
  (dolist (a args)
    (format t "~A " a))
  (format t " ***~%"))
	  
