


(global *cyco-error-as-warning* t)
(global *enable-warnings* t)


(labels ((error-banner
	  (msg)
	  (format t "~A ~A~%" +banner-error+ msg))

	 (warning-banner
	  (msg)
	  (format t "~A ~A~%" +banner-warning+ msg))
	 )


  (defun cyco-warning (&rest msg)
    (if *enable-warnings*
	(progn
	  (warning-banner (car msg))
	  (dolist (m msg)(format t "WARNING: ~A~%" m))
	  (format t "~%"))))

  (defun cyco-cue-warning (function-name args &rest more)
    (apply #'cyco-warning
	   (append (list 'cue-function-warning
			 (sformat "Cuing function : ~A" function-name)
			 (sformat "ARGS           : ~A" args))
		   more)))
  
  (defun cyco-error (&rest msg)
    (if *cyco-error-as-warning*
	(apply #'cyco-warning msg)
      (progn 
	(error-banner (car msg))
	(dolist (m msg)(format t "ERROR: ~A~%" m))
	(format t "~%")
	(error (->string (car msg))))))

  (defun cyco-type-error (function-name expected encounterd &rest more)
    (let ((args (append
		 (list "TYPE-ERROR"
		       (sformat "Function/method : ~A" function-name)
		       (sformat "Expected        : ~A" expected)
		       (sformat "Encounterd      : ~A ~A"
				(type-of encounterd) encounterd))
		 more)))
      (apply #'cyco-error args)))
			       
  (defun cyco-value-error (function-name offending-value &rest more)
    (let ((args (append
		 (list "VALUE-ERROR"
		       (sformat "Function/method : ~A" function-name)
		       (sformat "Offending value : ~A" offending-value))
		 more)))
      (apply #'cyco-error args)))

  (defun cyco-composition-error (function-name &rest msg)
    (apply #'cyco-error
	   (append (list 'composition-error
			 (sformat "Function name : ~A" function-name))
		   msg)))

  
  
  )
