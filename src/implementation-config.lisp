;;;; CYCO implementation-config.lisp
;;;;
;;;; Loads implementation specific source.
;;;;

(let ((imp (lisp-implementation-type))
      (ver (lisp-implementation-version)))
  (format t "CYCO ~A Lisp type ~A ~A~%"
		  +CYCO-VERSION+ imp ver)
  (cond ((string= imp "SBCL")
	 (load "sbcl-cyco"))
	((string= imp "Armed Bear Common Lisp")
	 (load "abcl-cyco"))
	;;((string= imp "MKCL")
	;;(load "src/mkcl/mkcl-cyco"))
	(t nil)))
