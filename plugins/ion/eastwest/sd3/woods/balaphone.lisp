;;;; CYCO plugins sj eastwest sd3 woods balaphone.lisp
;;;;

(param balaphone nil)

(let* ((low-note (ew-keynumber 'a2))
       (high-note (ew-keynumber 'd6)))
  (defun balaphone (&key (parent ew-sd3-woods)
			 (key-wrap-mode nil)
			 channel dynamic-map remarks)
    (setf balaphone (make-instrument 'balaphone
				     :transient t
				     :parent parent
				     :channel channel
				     :dynamic-map dynamic-map
				     :remarks remarks
				     :keynumber-map (if key-wrap-mode
							(wrapping-keynumber-map :min low-note
										:max high-note)
						      (basic-keynumber-map :min low-note
									   :max high-note))))
	(make-eastwest-program-map balaphone '((hits 0)
					       (flams 1)
					       (rolls 2)))
	balaphone))

(defun ?balaphone ()
  (format t "The CYCO BALAPHONE instrument is used for *ALL* Stormdrum3 Balaphone variations:~%")
  (format t "    Balaphone Hits~%")
  (format t "    Balaphone Flams~%")
  (format t "    Balaphone Roll DXF~%")
  (format t "    Balaphone KS C0-D0~%")
  (format t "There are two possible keynumber-maps~%")
  (format t "If :KEY-WRAP-MODE is true, out of bound notes are transposed back into range.~%")
  (format t "Otherwise out of bound notes are treated as rest.~%"))


(param gourd-marimba nil)

(let* ((low-note (ew-keynumber 'a0))
       (high-note (ew-keynumber 'd3)))
  (defun gourd-marimba (&key (parent ew-sd3-woods)
			     (key-wrap-mode nil)
			     channel dynamic-map
			     (remarks "SD3 Woods Gourd-Marimba"))
    (setf gourd-marimba (make-instrument 'gourd-marimba
					 :transient t
					 :parent parent
					 :channel channel
					 :dynamic-map dynamic-map
					 :remarks remarks
					 :keynumber-map (if key-wrap-mode
							    (wrapping-keynumber-map :min low-note
										    :max high-note)
							  (basic-keynumber-map :min low-note
									       :max high-note))))
    gourd-marimba))

