;;;; CYCO plugins sj eastwest sd3 metals dark-star.lisp
;;;;

(param dark-star-bells nil)

(let ((keymap-table (make-hash-table))
      (remark-table (make-hash-table)))
  (setf (gethash 'hits keymap-table)(circular-list-keynumber-map
				     (white-keys (ew-keynumber 'd3)
						 (ew-keynumber 'b4)))
	(gethash 'tuned keymap-table)(wrapping-keynumber-map
				      :min (ew-keynumber 'c3)
				      :max (ew-keynumber 'c6))
	(gethash 'perf keymap-table)(circular-list-keynumber-map
				     (white-keys (ew-keynumber 'c1)
						 (ew-keynumber 'd5))))
  (setf (gethash 'hits remark-table) "SD3 Metals Dark Star Bells  diatonic RR 2"
	(gethash 'tuned remark-table) "SD3 Metals Dark Star Bells tuned RR 2"
	(gethash 'perf remark-table) "SD3 Metals Dark Star Bells Perf  RR 1")
  (sd3-instrument dark-star-bells ew-sd3-metals keymap-table remark-table)
  (sd3-query ?dark-star-bells "Dark Star Bells (tubular)" remark-table))
				      
	
	
