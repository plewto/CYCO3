;;;; CYCO plugins sj eastwest sd3 gongs thai.lisp
;;;;

(param thai-gong nil)

(defun thai-gong (variation &key (parent ew-sd3-gongs)
			    channel dynamic-map remarks)
  (let ((low-key (ew-keynumber 'c1))
	(high-key nil)
	(rem nil))
    (cond ((eq variation 'hits)
	   (setf high-key (ew-keynumber 'a6))
	   (setf rem "36in Thai Gong Hits"))
	  ((eq variation 'rolls)
	   (setf high-key (ew-keynumber 'f5))
	   (setf rem "36in Thai Gong Rolls & Perfs"))
	  (t
	   (cyco-warning
	    (sformat "Invalid thai-gong variation ~A" variation)
	    "Using default 'hits")
	   (setf high-key (ew-keynumber 'a6))
	   (setf rem "36in Thai Gong Hits")))
    (setf thai-gong (make-instrument 'thai-gong
				     :transient t
				     :parent parent
				     :channel channel
				     :dynamic-map dynamic-map
				     :remarks (or remarks rem)
				     :keynumber-map (circular-list-keynumber-map
						     (white-keys low-key high-key))))))


(defun ?thai-gong ()
  (format t "SD3 Woohan-Gong~%")
  (format t "  Variation 'HITS   SD3 Gongs Clockworks Waterphons   36in Thai Gong Hits~%")
  (format t "  Variation 'ROLLS  SD3 Gongs Clockworks Waterphons   36in Thai Gong Rolls & PerfFX~%"))
  
  
