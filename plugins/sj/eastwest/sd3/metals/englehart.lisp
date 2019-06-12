;;;; CYCO plugins sj eastwest sd3 metals englehart.lisp
;;;;

(param englehart-bells nil)

(let ((keymap-table (make-hash-table))
      (remark-table (make-hash-table)))
  (setf (gethash 'bells keymap-table)(symbolic-keynumber-map
				      (list
				        (list 'HIT-1   (ew-keynumber 'C1 ))
					(list 'MUTE-1  (ew-keynumber 'D1 ))
					(list 'HIT-2   (ew-keynumber 'E1 ))
					(list 'MUTE-2  (ew-keynumber 'F1 ))
					(list 'HIT-3   (ew-keynumber 'G1 ))
					(list 'MUTE-3  (ew-keynumber 'A1 ))
					(list 'HIT-4   (ew-keynumber 'B1 ))
					(list 'MUTE-4  (ew-keynumber 'C2 ))
					(list 'HIT-5   (ew-keynumber 'D2 ))
					(list 'MUTE-5  (ew-keynumber 'E2 ))
					(list 'HIT-6   (ew-keynumber 'F2 ))
					(list 'MUTE-6  (ew-keynumber 'G2 ))
					(list 'STICK-1 (ew-keynumber 'C3 ))
					(list 'STICK-MUTE-1 (ew-keynumber 'D3 ))
					(list 'STICK-2      (ew-keynumber 'E3 ))
					(list 'STICK-MUTE-2 (ew-keynumber 'F3 ))
					(list 'STICK-3      (ew-keynumber 'G3 ))
					(list 'STICK-MUTE-3 (ew-keynumber 'A3 ))
					(list 'STICK-4      (ew-keynumber 'B3 ))
					(list 'STICK-MUTE-4 (ew-keynumber 'C4 ))
					(list 'STICK-5      (ew-keynumber 'D4 ))
					(list 'STICK-MUTE-5 (ew-keynumber 'E4 ))
					(list 'STICK-6      (ew-keynumber 'F4 ))
					(list 'STICK-MUTE-6 (ew-keynumber 'G4 ))))
	(gethash 'tuned keymap-table)(wrapping-keynumber-map :min (ew-keynumber 'f2)
							     :max (ew-keynumber 'e4))
	(gethash 'tuned-stick keymap-table)(wrapping-keynumber-map :min (ew-keynumber 'f2)
								   :max (ew-keynumber 'e4)))
  (setf (gethash 'bells remark-table) "SD3 Metals Englehart Bells Mt w Mutes  RR 4"
	(gethash 'tuned remark-table) "SD3 Metals Englehart Bells Tuned  RR 4"
	(gethash 'tuned-stick remark-table) "SD3 Metals Englehart Bells Tuned-stick  RR 4")
  (sd3-instrument englehart-bells ew-sd3-metals keymap-table remark-table)
  (sd3-query ?englehart-bells 'englehart-bells remark-table))

  
	
	
	
