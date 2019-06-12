;;;; CYCO plugins sj eastwest sd3 metals supra.lisp
;;;;

(param supra nil)

(let ((keymap-table (make-hash-table))
      (remark-table (make-hash-table)))
  (setf (gethash 'bowles keymap-table) (symbolic-keynumber-map
					(list
					 (list 'HIT-01  (ew-keynumber 'C1 ))
					 (list 'MUTE-01 (ew-keynumber 'D1 ))
					 (list 'HIT-02  (ew-keynumber 'E1 ))
					 (list 'MUTE-02 (ew-keynumber 'F1 ))
					 (list 'HIT-03  (ew-keynumber 'G1 ))
					 (list 'MUTE-03 (ew-keynumber 'A1 ))
					 (list 'HIT-04  (ew-keynumber 'B1 ))
					 (list 'MUTE-04 (ew-keynumber 'C2 ))
					 (list 'HIT-05  (ew-keynumber 'D2 ))
					 (list 'MUTE-05 (ew-keynumber 'E2 ))
					 (list 'HIT-06  (ew-keynumber 'F2 ))
					 (list 'MUTE-06 (ew-keynumber 'G2 ))
					 (list 'HIT-07  (ew-keynumber 'A2 ))
					 (list 'MUTE-07 (ew-keynumber 'B2 ))
					 (list 'HIT-08  (ew-keynumber 'C3 ))
					 (list 'MUTE-08 (ew-keynumber 'D3 ))
					 (list 'HIT-09  (ew-keynumber 'E3 ))
					 (list 'MUTE-09 (ew-keynumber 'F3 ))
					 (list 'HIT-10  (ew-keynumber 'G3 ))
					 (list 'MUTE-10 (ew-keynumber 'A3 ))
					 (list 'HIT-11  (ew-keynumber 'B3 ))
					 (list 'MUTE-11 (ew-keynumber 'C4 ))
					 (list 'HIT-12  (ew-keynumber 'D4 ))
					 (list 'MUTE-12 (ew-keynumber 'E4 ))
					 (list 'HIT-13  (ew-keynumber 'F4 ))
					 (list 'MUTE-13 (ew-keynumber 'G4 ))
					 (list 'HIT-14  (ew-keynumber 'A4 ))
					 (list 'MUTE-14 (ew-keynumber 'B4 ))
					 (list 'HIT-15  (ew-keynumber 'C5 ))
					 (list 'MUTE-15 (ew-keynumber 'D5 ))
					 (list 'HIT-16  (ew-keynumber 'E5 ))
					 (list 'MUTE-16 (ew-keynumber 'F5 ))
					 (list 'HIT-17  (ew-keynumber 'G5 ))
					 (list 'MUTE-17 (ew-keynumber 'A5 ))
					 (list 'MUTE-18 (ew-keynumber 'B5 ))
					 (list 'PERF-1  (ew-keynumber 'C5 ))
					 (list 'PERF-2  (ew-keynumber 'D5 ))
					 (list 'PERF-3  (ew-keynumber 'E5 ))
					 (list 'PERF-4  (ew-keynumber 'F5 ))))

	(gethash 'fx keymap-table) (symbolic-keynumber-map
				    (list
				     (list 'FX-01 (ew-keynumber 'C1 ))
				     (list 'FX-02 (ew-keynumber 'D1 ))
				     (list 'FX-03 (ew-keynumber 'E1 ))
				     (list 'FX-04 (ew-keynumber 'F1 ))
				     (list 'FX-05 (ew-keynumber 'G1 ))
				     (list 'FX-06 (ew-keynumber 'A1 ))
				     (list 'FX-07 (ew-keynumber 'B1 ))
				     (list 'FX-08 (ew-keynumber 'C2 ))
				     (list 'FX-09 (ew-keynumber 'D2 ))
				     (list 'FX-10 (ew-keynumber 'E2 ))
				     (list 'FX-11 (ew-keynumber 'F2 ))
				     (list 'FX-12 (ew-keynumber 'G2 ))
				     (list 'FX-13 (ew-keynumber 'A2 ))
				     (list 'PERF-1 (ew-keynumber 'C3 ))
				     (list 'PERF-2 (ew-keynumber 'D3 )))))
  (setf (gethash 'bowels remark-table) "SD3 Metals Supra Bowls"
	(gethash 'fx remark-table) "SD3 Metals Supra FX")
  
  (sd3-instrument supra ew-sd3-metals keymap-table remark-table)
  (sd3-query ?supra 'supra remark-table)) 
