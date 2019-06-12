;;;; CYCO plugins sj sd3 dumbek.lisp
;;;;
;;;; ew-sd3-bigdrums
;;;;  |
;;;;  +-- dumbek
;;;;          ashiko       Big Drums
;;;;          egypt        Big Drums
;;;;          remo-9       Small Drums
;;;;

(param dumbek nil)

(let ((keymap-table (make-hash-table))
      (remarks-table (make-hash-table)))
  (setf (gethash 'ashiko keymap-table)
	(symbolic-keynumber-map
	 (list (list 'BASS        (ew-keynumber 'C1))
	       (list 'BASS-SCRAPE (ew-keynumber 'D1))
	       (list 'MUTE        (ew-keynumber 'E1))
	       (list 'MUTE-SLAP   (ew-keynumber 'F1))
	       (list 'RIM   (ew-keynumber 'G1))
	       (list 'FLAM  (ew-keynumber 'A1))
	       (list 'FX-01 (ew-keynumber 'B1))
	       (list 'FX-02 (ew-keynumber 'C2))
	       (list 'FX-03 (ew-keynumber 'D2))
	       (list 'FX-04 (ew-keynumber 'E2))
	       (list 'FX-05 (ew-keynumber 'F2))
	       (list 'FX-06 (ew-keynumber 'G2))
	       (list 'FX-07 (ew-keynumber 'A2))
	       (list 'FX-08 (ew-keynumber 'B2))
	       (list 'FX-09 (ew-keynumber 'C3))
	       (list 'FX-10 (ew-keynumber 'D3))
	       (list 'FX-11 (ew-keynumber 'E3))
	       (list 'PERF  (ew-keynumber 'F3)))))
  (setf (gethash 'ashiko remarks-table) "SD3 Big Drums African Ceramic Ashiko")
  (setf (gethash 'egyptian keymap-table)
	(symbolic-keynumber-map
	 (list (list 'CENTER       (ew-keynumber 'C1))
	       (list 'BASS-SLIDE   (ew-keynumber 'D1))
	       (list 'MUTE-LH      (ew-keynumber 'E1))
	       (list 'MUTE-RH      (ew-keynumber 'F1))
	       (list 'RIM          (ew-keynumber 'G1))
	       (list 'FLAM         (ew-keynumber 'A1))
	       (list 'SLAP-1       (ew-keynumber 'B1))
	       (list 'SLAP-2       (ew-keynumber 'C2))
	       (list 'FINGER-EDGE  (ew-keynumber 'D2))
	       (list 'FINGER-SNAP  (ew-keynumber 'E2))
	       (list 'SHORT-ROLL-1 (ew-keynumber 'F2))
	       (list 'SHORT-ROLL-2 (ew-keynumber 'G2))
	       (list 'LONG-ROLL-0  (ew-keynumber 'A2))
	       (list 'LONG-ROLL-1  (ew-keynumber 'B2))
	       (list 'LONG-ROLL-2  (ew-keynumber 'C3))
	       (list 'LONG-ROLL-3  (ew-keynumber 'D3))
	       (list 'PERF         (ew-keynumber 'E3)))))
  (setf (gethash 'egyptian remarks-table) "SD3 Big Drums Dumbek Egypt 1972")
  (setf (gethash 'remo-9 keymap-table)
	(symbolic-keynumber-map
	 (list (list 'BASS-HIT        (ew-keynumber 'C1)) 
	       (list 'OPEN-OFF-CENTER (ew-keynumber 'D1)) 
	       (list 'FLAM            (ew-keynumber 'E1)) 
	       (list 'OPEN-SLAP       (ew-keynumber 'F1)) 
	       (list 'RIM             (ew-keynumber 'G1)) 
	       (list 'SLAP-FLAM       (ew-keynumber 'A1)) 
	       (list 'MUTE-CENTER     (ew-keynumber 'B1)) 
	       (list 'MUTE-OFF-CENTER (ew-keynumber 'C2)) 
	       (list 'FINGER-ROLL-1   (ew-keynumber 'D2)) 
	       (list 'FINGER-ROLL-2   (ew-keynumber 'E2)) 
	       (list 'ROUGH-ROLL      (ew-keynumber 'F2)))))
  (setf (gethash 'remo-9 remarks-table) "SD3 Small Drum Remo 9in Dumbek Skindeep")
  (sd3-instrument dumbek ew-sd3-bigdrums keymap-table remarks-table)
  (sd3-query ?dumbek 'dumbek remarks-table))
  
