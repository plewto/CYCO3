;;;; CYCO3 util/notifications
;;;;
;;;; Defines CYCO warnings and errors.
;;;; 


(global *cyco-error-as-warning* t
	"If true treat errors as warnings.")

(global *enable-warnings* t
	"If nil suppress warning messages.")

(labels ((error-banner
	  (msg)
	  (format t "~A ~A~%" +banner-error+ msg))

	 (warning-banner
	  (msg)
	  (format t "~A ~A~%" +banner-warning+ msg)))

  (defun cyco-warning (&rest msg)
    "Display warning message."
    (if *enable-warnings*
	(progn
	  (warning-banner (car msg))
	  (dolist (m msg)(format t "WARNING: ~A~%" m))
	  (format t "~%"))))

  (defun cyco-cue-warning (function-name args &rest more)
    "Display warning message that argument to cuing function 
is invalid."
    (apply #'cyco-warning
	   (append (list 'cue-function-warning
			 (sformat "Cuing function : ~A" function-name)
			 (sformat "ARGS           : ~A" args))
		   more)))
  
  (defun cyco-error (&rest msg)
    "Generalized error.
Displays message and then terminates CYCO.
If *CYCO-ERROR-AS-WARNING* is true, the error is treated as 
a warning and CYCO does not terminate."
    (if *cyco-error-as-warning*
	(apply #'cyco-warning msg)
      (progn 
	(error-banner (car msg))
	(dolist (m msg)(format t "ERROR: ~A~%" m))
	(format t "~%")
	(error (->string (car msg))))))

  (defun cyco-type-error (function-name expected encounterd &rest more)
    "Error indicating wrong type was passed to a function."
    (let ((args (append
		 (list "TYPE-ERROR"
		       (sformat "Function/method : ~A" function-name)
		       (sformat "Expected        : ~A" expected)
		       (sformat "Encountered     : ~A ~A"
				(type-of encounterd) encounterd))
		 more)))
      (apply #'cyco-error args)))
			       
  (defun cyco-value-error (function-name offending-value &rest more)
    "Error indicating that a function argument is out of bounds."
    (let ((args (append
		 (list "VALUE-ERROR"
		       (sformat "Function/method : ~A" function-name)
		       (sformat "Offending value : ~A" offending-value))
		 more)))
      (apply #'cyco-error args)))

  (defun cyco-composition-error (function-name &rest msg)
    "Error indicating that something is wrong with a composition definition."
    (apply #'cyco-error
	   (append (list 'composition-error
			 (sformat "Function name : ~A" function-name))
		   msg))) )
