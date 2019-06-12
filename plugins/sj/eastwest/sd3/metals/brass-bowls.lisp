;;;; CYCO plugins sj eastwest sd3 metals brass-bowls.lisp
;;;;
;;;; Vraiations: tibet, turkey
;;;;

(param brass-bowls nil)

(let ((keymap-table (make-hash-table))
      (remark-table (make-hash-table)))
  (setf (gethash 'tibet keymap-table)
	(circular-list-keynumber-map
	 (white-keys (ew-keynumber 'c1)
		     (ew-keynumber 'b6))))
  (setf (gethash 'tibet remark-table) "SD3 Metals Brass Bowls Tibet")
  (setf (gethash 'turkey keymap-table)
	(circular-list-keynumber-map
	 (white-keys (ew-keynumber 'c1)
		     (ew-keynumber 'e5))))
  (setf (gethash 'turkey remark-table) "SD3 Metals Brass Bowls Turkey")
  (sd3-instrument brass-bowls ew-sd3-metals keymap-table remark-table)
  (sd3-query ?brass-bowls 'brass-bowls remark-table))
