;;;; CYCO cyco.lisp
;;;;
;;;; Main entry point to build CYCO.
;;;;

(defpackage :cyco
  (:use :cl))

(in-package :cyco)

(defun set-cyco-prompt ())

(defmacro constant (name value &optional (docstring ""))
  "An alias for defconstant"
  `(if (not (boundp ',name))
       (defconstant ,name ,value ,docstring)))

(defmacro param (name &optional (value nil)(docstring ""))
  "An alias for defparameter."
  `(defparameter ,name ,value ,docstring))

(defmacro global (name &optional (value nil)(docstring ""))
  "An alias for defparameter."
  `(defparameter ,name ,value ,docstring))

(defmacro while (test &rest body)
  `(do ()
       ((not ,test))
     ,@body))

(defun bool (object)
  "Returns canonical boolean t or nil."
  (if object t nil))

(defmacro constant-function (name value)
  `(defun ,name (&rest args)
     (declare (ignore args))
     (format nil "Returns constant ~A" ,value)
     ,value))

(constant-function true t)
(constant-function false nil)
(load "src/bootstrap")
  
(defun ?version ()
  "Displays current version (major minor revision))"
  (format t "Version: ~A~%" +cyco-version+))

(defun version (major &optional minor)
  "Enforces version.
If either major dose not match current major version number, 
or minor is specified and it is less then current minor version number,
a warning message is displayed and CYCO terminates."
  (let ((mj (eq major (car +cyco-version+)))
	(mn  (or (not minor)
		 (>= minor (second +cyco-version+)))))
    (if (not (and mj mn))
	(progn 
	  (cyco-warning "CYCO version mismatch"
			(sformat "Current version   : ~A" +cyco-version+)
			(sformat "Specified version : (~A ~A NIL)" major minor))
	  (exit)))))

(in-package :common-lisp-user)

(defun cyco ()
  "Switch to CYCO namespace"
  (in-package :cyco)
  (cyco::set-cyco-prompt)
  (cyco::cyco-banner)
  nil)

(in-package :cyco)

(defun cyco ()
  (cyco-banner)
  (set-cyco-prompt)
  nil)



