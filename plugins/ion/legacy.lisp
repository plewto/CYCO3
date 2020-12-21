;;;; CYCO plugins ion legacy
;;;;

(defmacro renamed-function-warning (old-name new-name)
  `(defun ,old-name (&rest _)
     (declare (ignore _))
     (cyco-warning "Function Renamed"
		   (sformat "~A has been renamed to ~A"
			    ',old-name ',new-name))
     (exit)))

(renamed-function-warning seq-order section-order)

