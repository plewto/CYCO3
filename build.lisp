;;;; build.lisp
;;;; Loads cyco source files
;;;;

(load "src/cyco")

(defparameter *no-debugger* t)

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
			       (declare (ignore h))
			       (print c)
			       (abort)))))
