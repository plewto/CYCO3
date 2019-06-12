;;;; CYCO plugins sj eastwest sd3 gongs waterphones.lisp
;;;;

(param waterphone nil)

(defun waterphone (variation &key (parent ew-sd3-gongs)
			     channel dynamic-map remarks)
  (let* ((low-key (ew-keynumber 'c1))
	 (vpair (cond ((eq variation 1)
		       (cons 'c6 "variation: 1"))
		      ((eq variation 2)
		       (cons 'fs6 "variation: 2"))
		      ((eq variation 3)
		       (cons 'e3 "variation: 3"))
		      ((eq variation 'percussion)
		       (cons 'a4 "variation: percussion"))
		      (t (cyco-warning
			  (sformat "Invalid waterphon variation:  ~A" variation)
			  "Defaulting to variation 1")
			 (setf variation 1)
			 (cons 'c6 "variation 1"))))
	 (high-key (ew-keynumber (car vpair)))
	 (rem (or remarks (cdr vpair))))
    (setf waterphone (make-instrument 'waterphone
				      :transient t
				      :parent parent
				      :channel channel
				      :dynamic-map dynamic-map
				      :keynumber-map (circular-list-keynumber-map
						      (range low-key (1+ high-key)))
				      :remarks rem))))
		       
(defun ?waterphone ()
  (format t "SD3 Waterphone~%")
  (dolist (v '(1 2 3 percussion))
    (format t "   Variation ~12A SD3 Gongs Clockwork Waterphons   Waterphone ~A~%" v v))) 
    
