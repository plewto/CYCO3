;;;; CYCO3 src/notifications
;;;;

(constant +BANNER-BAR1+ (scopies 68 #\*))
(constant +BANNER-HEADER1+ "*** ")
(constant +BANNER-BAR2+ (scopies 68 #\-))
(constant +BANNER-HEADER2+ "--- ")

(constant +BANNER-ERROR+ (sformat "ERROR: ~A" (scopies 30 #\*)))

(defun cyco-banner ()
  (format t "~%~A~%" +BANNER+)
  (format t "Version ~A~%" +CYCO-VERSION+))

(defun cr ()
  (format t "~%"))

(flet ((banner-bar (q)
		   (format t "~A~%" q))
       (banner-headline (text header width)
			(format t "~A~A~%" header (center-string text width -6)))
       (banner-more (text-list header)
		    (if text-list
			(format t "~A~%" header))
		      (dolist (q text-list)
			(format t "~A~A~%" header q))))
  (defun banner1 (headline &rest more)
    (banner-bar +banner-bar1+)
    (banner-headline headline +banner-header1+ 75)
    (banner-more more +banner-header1+)
    (banner-bar +banner-bar1+)
    (cr))

  (defun banner2 (headline &rest more)
    (banner-bar +banner-bar2+)
    (banner-headline headline +banner-header2+ 75)
    (banner-more more +banner-header2+)
    (cr))

  (defun banner3 (headline &rest more)
    (format t "~A~A~%" +banner-header2+ headline)
    (dolist (q more)
      (format t "~A   ~A~%" +banner-header2+ q))
    (cr)) )

;; If true display error messages only, do not call Lisp error function
;; If nil display messages and call Lisp error function
;;
(global *cyco-error-as-warning* t)

(labels ((error-banner (error-type)
		       (format t "~A ~A~%" +banner-error+ error-type))
	 (error-message (text)
			(format t "ERROR: ~A~%" text))
	 (cyco-error (message)
		     (if (not *cyco-error-as-warning*)
			 (error message))
		     (cr)
		     nil)
	 (print-more (message-list)
		     (dolist (txt message-list)
		       (error-message txt))))
       (defun cyco-type-error (function-name expected encounterd &rest more)
	 (error-banner 'cyco-type-error)
	 (error-message (sformat "Function   : ~A" function-name))
	 (error-message (sformat "Expected   : ~A" expected))
	 (error-message (sformat "Encounterd : ~A ~A" (type-of encounterd) encounterd))
	 (print-more more)
	 (cyco-error 'cyco-type-error))
       
       (defun cyco-value-error (function-name offending-value &rest more)
	 (error-banner 'cyco-value-error)
	 (error-message (sformat "Function : ~A" function-name))
	 (error-message (sformat "Value    : ~A" offending-value))
	 (print-more more)
	 (cyco-error 'cyco-value-error))

       (defun cyco-metric-expression-error (expression &rest more)
	 (error-banner 'cyco-metric-expression-error)
	 (error-message (sformat "Expression: ~A" expression))
	 (print-more more)
	 (cyco-error 'cyco-mertic-expression-error))
       
       (defun cyco-not-implemented-error (function-name object &rest more)
	 (error-banner 'cyco-not-implemented-error)
	 (error-message (sformat "Function  : ~A" function-name))
	 (error-message (sformat "Argument : ~A ~A" (type-of object)(->string object)))
	 (print-more more)
	 (cyco-error 'cyco-not-implemented-error))

       (defun cyco-property-error (node property &rest more)
	 (error-banner 'cyco-property-error)
	 (error-message (sformat "type     : ~A" (type-of node)))
	 (error-message (sformat "name     : ~A" (name node)))
	 (error-message (sformat "property : ~A" property))
	 (print-more more)
	 (cyco-error 'cyco-property-error))
       
       (defun cyco-circular-assignment-error (function-name value &rest more)
	 (error-banner 'cyco-circular-assignment-error)
	 (error-message (sformat "Function : ~A" function-name))
	 (error-message (sformat "Value    : ~A" (name value)))
	 (print-more more)
	 (cyco-error 'cyco-circular-assignment-error))
       
       (defun cyco-invalid-midi-track-error (&rest more)
	 (error-banner 'cyco-invalid-midi-track-error)
	 (print-more more)
	 (cyco-error 'cyco-invalid-midi-track-error))
       
       (defun cyco-no-project-error (function-name &rest more)
	 (error-banner 'cyco-no-project-error)
	 (error-message (sformat "function : ~A" function-name))
	 (print-more more)
	 (cyco-error 'cyco-no-project-error))
       
       (defun cyco-section-does-not-exists-error (function-name section-name &rest more)
	 (error-banner 'cyco-section-does-not-exists-error)
	 (error-message (sformat "Function : ~A" function-name))
	 (error-message (sformat "Section  : ~A" section-name))
	 (print-more more)
	 (cyco-error 'cyco-section-does-not-exists-error))

       (defun cyco-part-does-not-exists-error (function-name section-name part-name &rest more)
	 (error-banner 'cyco-part-does-not-exists-error)
	 (error-message (sformat "Function : ~A" function-name))
	 (error-message (sformat "Section  : ~A" section-name))
	 (error-message (sformat "Part     : ~A" part-name))
	 (print-more more)
	 (cyco-error 'cyco-part-does-not-exists-error))

       (defun cyco-malformed-part-event-error (part event-spec &rest more)
	 (let ((sname (name (parent part)))
	       (pname (name part)))
	   (error-banner 'cyco-malformed-part-event-error)
	   (error-message (sformat "Section : ~A" sname))
	   (error-message (sformat "Part    : ~A" pname))
	   (error-message (sformat "Event   : ~A" event-spec))
	   (print-more more)
	   (cyco-error 'cyco-malformed-part-event-error))) )

(global *enable-warnings* t
       "If true symbolic keynumber maps display warnings.")

(defun cyco-warning (msg &rest more)
  (if *enable-warnings*
      (progn 
	(format t "WARNING: ~A~%" msg)
	(dolist (msg more)(format t "WARNING:   ~A~%" msg)))))

(defun cyco-cue-warning (function-name args &rest more)
  (if *enable-warnings*
      (progn 
	(format t "~A WARNING:~%" +banner-bar1+)
	(format t "CYCO-CUE-WARNING function : ~A~%" function-name)
	(format t "CYCO-CUE-WARNING arguments: ~A~%" args)
	(dolist (q more) (format t "WARNING: ~A~%" q))
	(cr))))

(defun cyco-undefined-chord-warning (chord-name &rest more)
  (if *enable-warnings*
      (progn
	(format t "~A WARNING~%" +banner-bar1+)
	(format t "WARNING Undefined chord: ~A~%" chord-name)
	(dolist (q more)(format t "WARNING: ~A~%" q))
	(cr))))
	

