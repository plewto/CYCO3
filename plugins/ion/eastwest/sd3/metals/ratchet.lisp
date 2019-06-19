;;;; CYCO plugins sj eastwest sd3 metals rachet.lisp
;;;;

(param ratchet nil)

(let ((keymap-table (make-hash-table))
      (remark-table (make-hash-table)))
  (setf (gethash 'ratchet keymap-table) (circular-list-keynumber-map
					 (white-keys (ew-keynumber 'c1)
						     (ew-keynumber 'a4)))
	(gethash 'indonesian keymap-table) (circular-list-keynumber-map
					    (white-keys (ew-keynumber 'c1)
							(ew-keynumber 'a5))))
  (setf (gethash 'ratchet remark-table) "SD3 Metals Ratchet"
	(gethash 'indonesian remark-table) "SD3 Metals Small Indonesian Ratchet")

  (sd3-instrument ratchet ew-sd3-metals keymap-table remark-table)
  (sd3-query ?ratchet 'ratchet remark-table))
