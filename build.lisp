;;;; build.lisp
;;;; Loads cyco source files
;;;;

(load "src/cyco")

(defparameter *no-debugger* nil)

(format t "~%")
(format t "*********************************************~%")
(format t "***                                       ***~%")
(format t "***    Enter (CYCO) at the Lisp prompt    ***~%")
(format t "***                                       ***~%")
(format t "*********************************************~%")

(if *no-debugger*
    (progn
      (format t "~%Debugger disabled~%~%")
      (setf *debugger-hook* #'(lambda (c h)
				(format t "~%")
			       (dotimes (i 3)
				 (format  t "ERROR *************************** ERROR~%"))
			       (format t "~%")
			       (print c)
			       (print h)
			       (format t "~%")
			       (abort)))))
