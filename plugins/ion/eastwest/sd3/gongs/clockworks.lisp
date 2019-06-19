;;;; CYCO plugins sj eastwest sd3 gongs clockworks.lisp
;;;;

(param clockworks nil)

(defun clockworks (variation &key (parent ew-sd3-gongs)
			     channel dynamic-map remarks)
  (let* ((low-key (ew-keynumber 'c1))
	 (vpair (cond ((eq variation 1)
		       (cons 'c3 "variation 1"))
		      ((eq variation 2)
		       (cons 'e1 "variation 2"))
		      ((eq variation 3)
		       (cons 'd1 "variation 3"))
		      (t (cyco-warning
			  (sformat "Invalid clockworks variation: ~A" variation)
			  "Defaulting to 1")
			 (setf variation 1)
			 (cons 'c3 "variation 1"))))
	 (high-key (ew-keynumber (car vpair)))
	 (rem (or remarks (cdr vpair))))
    (setf clockworks (make-instrument
		      'clockworks
		      :transient t
		      :parent parent
		      :channel channel
		      :dynamic-map dynamic-map
		      :keynumber-map (circular-list-keynumber-map
				      (white-keys low-key high-key))
		      :remarks rem))))


(defun ?clockworks ()
  (format t "SD3 Clockworks~%")
  (dolist (v '(1 2 3))
    (format t "   Variation ~A   SD3 Gongs Clockwork Waterphons   Clockwork ~A fullmix~%" v v))) 
    

