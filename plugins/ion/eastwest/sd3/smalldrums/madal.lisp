;;;; CYCO plugins sj eastwest sd3 smalldrums madal.lisp
;;;;

(param madal nil)

(let ((keymap-table (make-hash-table))
      (remark-table (make-hash-table)))
  (setf (gethash 'micky keymap-table)(symbolic-keynumber-map
   				      (list
   				       (list 'CS-CENTER (ew-keynumber 'C1 ))
   				       (list 'CS-RIM    (ew-keynumber 'D1 ))
   				       (list 'CS-MUTE   (ew-keynumber 'E1 ))
   				       (list 'BF-CENTER (ew-keynumber 'F1 ))
   				       (list 'BF-RIM    (ew-keynumber 'G1 ))
   				       (list 'BF-MUTE   (ew-keynumber 'A1 ))))
  	(gethash 'generic keymap-table)(symbolic-keynumber-map
  				      (list
  				       (list 'OPEN    (ew-keynumber 'C1 ))
  				       (list 'RIM     (ew-keynumber 'D1 ))
  				       (list 'FLAM    (ew-keynumber 'E1 ))
  				       (list 'SLAP    (ew-keynumber 'F1 ))
  				       (list 'ROLL-P  (ew-keynumber 'G1 ))
  				       (list 'ROLL-MP (ew-keynumber 'A1 ))
  				       (list 'ROLL-MF (ew-keynumber 'B1 ))
  				       (list 'ROLL-F  (ew-keynumber 'C2 ))))
  	(gethash 'nepal keymap-table)(symbolic-keynumber-map
  				      (list
  				       (list 'OPEN      (ew-keynumber 'C1 ))
  				       (list 'RIM       (ew-keynumber 'D1 ))
  				       (list 'LOW-HIGH  (ew-keynumber 'E1 ))
  				       (list 'HIGH-LOW  (ew-keynumber 'F1 ))
  				       (list 'LOW-OPEN  (ew-keynumber 'G1 ))
  				       (list 'HIGH-OPEN (ew-keynumber 'A1 ))
  				       (list 'HIGH-MUTE (ew-keynumber 'B1 ))
  				       (list 'MUTE      (ew-keynumber 'C2 ))))
  	)
  (setf (gethash 'micky remark-table) "SD3 SmallDrums Madal Micky"
	(gethash 'generic remark-table) "SD3 SmallDrums Madales"
	(gethash 'nepal remark-table) "SD3 SmallDrums Napalese Madal")
  (sd3-instrument madal ew-sd3-smalldrums keymap-table remark-table)
  (sd3-query ?madal 'madal remark-table))

