;;;; CYCO plugins sj eastweest sd3 metals doorstop
;;;;

(param doorstop nil)

(let ((keymap-table (make-hash-table))
      (remark-table (make-hash-table)))
  (setf (gethash 1 keymap-table)(circular-keynumber-map
				 (ew-keynumber 'c1)
				 (ew-keynumber 'e6))
	(gethash 2 keymap-table)(circular-keynumber-map
				 (ew-keynumber 'c1)
				 (ew-keynumber 'cs6))
	(gethash 3 keymap-table)(circular-keynumber-map
				 (ew-keynumber 'c1)
				 (ew-keynumber 'b3)))
  (setf (gethash 1 remark-table) "SD3 Metals DoorStop 1"
	(gethash 2 remark-table) "SD3 Metals DoorStop 2"
	(gethash 3 remark-table) "SD3 Metals DoorStop 3")
  (sd3-instrument doorstop ew-sd3-metals keymap-table remark-table)
  (sd3-query ?doorstop 'doorstop remark-table))
