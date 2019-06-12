;;;; CYCO plugins sj eastweest sd3 smalldrums cuica.lisp
;;;;

(param cuica nil)

(let ((keymap-table (make-hash-table))
      (remark-table (make-hash-table)))
  (setf (gethash 1 keymap-table)(circular-list-keynumber-map
				 (white-keys (ew-keynumber 'c1)
					     (ew-keynumber 'c2)))
	(gethash 2 keymap-table)(circular-list-keynumber-map
				 (append 
				  (white-keys (ew-keynumber 'c1)
					      (ew-keynumber 'c2))
				  (white-keys (ew-keynumber 'c3)
					      (ew-keynumber 'a3)))))
  (setf (gethash 1 remark-table) "SD3 SmallDrums  Cuica"
	(gethash 2 remark-table) "SD3 SmallDrums  Cuica Perf FX")
  (sd3-instrument cuica ew-sd3-smalldrums keymap-table remark-table)
  (sd3-query ?cuica 'cuica remark-table)) 
				 
