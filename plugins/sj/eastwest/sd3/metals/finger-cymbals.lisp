;;;; CYCO plugins sj eastwest sd3 metals finger.lisp
;;;;

(param finger-cymbals nil)

(let ((keymap-table (make-hash-table))
      (remark-table (make-hash-table)))
  (setf (gethash 1 keymap-table)(circular-list-keynumber-map
				 (white-keys (ew-keynumber 'c1)
					     (ew-keynumber 'e3)))
	(gethash 2 keymap-table)(circular-list-keynumber-map
				 (white-keys (ew-keynumber 'c1)
					     (ew-keynumber 'g2))))
  (setf (gethash 1 remark-table) "SD3 Metals Finger Cymbals 1  RR 6"
	(gethash 2 remark-table) "SD3 Metals Finger Cymbals 2  RR 4")

  (sd3-instrument finger-cymbals ew-sd3-metals keymap-table remark-table)
  (sd3-query ?finger-cymbals 'finger-cymbals remark-table))

  
	
	
	
