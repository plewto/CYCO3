;;;; CYCO plugins ion legacy
;;;;

(defmacro renamed-function-warning (old-name new-name)
  `(defun ,old-name (&rest _)
     (dismiss _)
     (cyco-warning "Function Renamed"
		   (sformat "~A has been renamed to ~A"
			    ',old-name ',new-name))
     (exit)))

(renamed-function-warning seq-order section-order)


;; Replacement for CYCO2 bar function
;;
(defun legacy-bar (time-signature time-specification)
  (let* ((time-specification-vector (->vector (fill-list (->list time-specification)
							 '(1 1 1 0))))
	 (bar (aref time-specification-vector 0))
	 (beat (aref time-specification-vector 1))
	 (subbeat (aref time-specification-vector 2))
	 (tick (aref time-specification-vector 3)))
    (+ (* bar (bar-duration time-signature))
       (* beat (beat-duration time-signature))
       (* subbeat (subbeat-duration time-signature))
       (* tick (tick-duration time-signature)))))
