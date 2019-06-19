;;;; CYCO plugins sj eastwest gongs woohan.lisp
;;;;

(param woohan-gong nil)

(defun woohan-gong (variation &key (parent ew-sd3-gongs)
			      channel dynamic-map remarks)
  (let* ((low-key (ew-keynumber 'c1))
	 (vpair (cond ((eq variation 10)
		       (cons 'e3 "10-inch Woohan Gong"))
		      ((eq variation 21)
		       (cons 'g4 "21-inch Woohan Gong"))
		      ((eq variation 35)
		       (cons 'e2 "35-inch Woohan Gong"))
		      ((eq variation '22-bowed)
		       (cons 'c4 "22-inch Bowed Woohan Cymbal"))
		      (t (cyco-warning
			  (sformat "Invalid woohan-gong variation:  ~A" variation)
			  "Using default 10-inch")
			 (setf variation 10)
			 (cons 'e3 "10-inch Woohan Gong"))))
	 (high-key (ew-keynumber (car vpair)))
	 (rem (or remarks (cdr vpair))))
    (setf woohan-gong (make-instrument 'woohan-gong
				       :transient t
				       :parent parent
				       :channel channel
				       :dynamic-map dynamic-map
				       :keynumber-map (circular-list-keynumber-map
						       (white-keys low-key high-key))
				       :remarks rem))))
				       
(defun ?woohan-gong ()
  (format t "SD3 Woohan-Gong~%")
  (dolist (v '(10 21 35 22-bowed))
    (if (member v '(10 21 35))
	(format t "  Variation ~8A SD3 Gongs Clockworks Waterphons  ~Ain Woohan Gong~%" v v)
        (format t "  Variation ~8A SD3 Gongs Clockworks Waterphons  22in Woohan Bowed Gong~%" v))))
