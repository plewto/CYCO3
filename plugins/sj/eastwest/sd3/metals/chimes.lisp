;;;; CYCO plugins sj eastwest metals chimes.lisp
;;;;

(param chimes nil)

(let ((keymap-table (make-hash-table))
      (remark-table (make-hash-table)))

  (setf (gethash 'brass keymap-table) (circular-list-keynumber-map
				       (white-keys (ew-keynumber 'c1)
						   (ew-keynumber 'g1)))
	(gethash 'burma keymap-table) (circular-list-keynumber-map
				       (white-keys (ew-keynumber 'c1)
						   (ew-keynumber 'f4)))
	(gethash 'cymbal keymap-table) (circular-list-keynumber-map
					(white-keys (ew-keynumber 'c1)
						    (ew-keynumber 'c2)))
	(gethash 'wind keymap-table) (circular-list-keynumber-map
				      (white-keys (ew-keynumber 'c1)
						  (ew-keynumber 'c4))))
  (setf (gethash 'brass remark-table) "SD3 Metals Chimes Brass RR 1"
	(gethash 'burma remark-table) "SD3 Metals Chimes Burma RR 1"
	(gethash 'cymbal remark-table) "SD3 Metals Chimes Cymbal RR 1"
	(gethash 'wind remark-table) "SD3 Metals Chimes Large Wind  RR 1")
  (sd3-instrument chimes ew-sd3-metals keymap-table remark-table)
  (sd3-query ?chimes 'chimes remark-table))
	
	
	
	
