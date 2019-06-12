;;;; CYCO plugins sj eastwest sd3 taiko okedo.lisp
;;;; 20 26

(param okedo nil)

(let ((keymap-table (make-hash-table))
      (remark-table (make-hash-table)))
  (setf
   (gethash 20 keymap-table)(symbolic-keynumber-map
			     (list
			      (list 'HIT-LH  (ew-keynumber 'C1)) 
			      (list 'HIT-RH  (ew-keynumber 'D1)) 
			      (list 'FLAM    (ew-keynumber 'E1)) 
			      (list 'ROLL-01 (ew-keynumber 'F1)) 
			      (list 'ROLL-02 (ew-keynumber 'G1)) 
			      (list 'ROLL-03 (ew-keynumber 'A1)) 
			      (list 'ROLL-04 (ew-keynumber 'B1)) 
			      (list 'ROLL-05 (ew-keynumber 'C2)) 
			      (list 'ROLL-06 (ew-keynumber 'D2)) 
			      (list 'ROLL-07 (ew-keynumber 'E2)) 
			      (list 'ROLL-08 (ew-keynumber 'F2)) 
			      (list 'ROLL-09 (ew-keynumber 'G2)) 
			      (list 'ROLL-10 (ew-keynumber 'A2)))) 
   (gethash 26 keymap-table)(symbolic-keynumber-map
			     (list
			      (list 'HIT-LH    (ew-keynumber 'C1)) 
			      (list 'HIT-RH    (ew-keynumber 'D1)) 
			      (list 'FLAM      (ew-keynumber 'E1)) 
			      (list 'ROLL-SHOT (ew-keynumber 'F1)) 
			      (list 'RIM-LH    (ew-keynumber 'G1)) 
			      (list 'RIM-RH    (ew-keynumber 'A1)) 
			      (list 'RIM-FLAM  (ew-keynumber 'B1)) 
			      (list 'RIM-FLAM-DOUBLE (ew-keynumber 'C2)) 
			      (list 'RIM-SHORT-ROLL  (ew-keynumber 'D2)) 
			      (list 'ROLL-1 (ew-keynumber 'E2) "Crescendo roll") 
			      (list 'ROLL-2 (ew-keynumber 'F2) "Crescendo roll") 
			      (list 'ROLL-3 (ew-keynumber 'G2) "Crescendo roll") 
			      (list 'ROLL-4 (ew-keynumber 'A2) "Crescendo roll") 
			      (list 'ROLL-5 (ew-keynumber 'B2) "Crescendo roll") 
			      (list 'ROLL-6 (ew-keynumber 'C3) "Crescendo roll") 
			      (list 'ROLL-7 (ew-keynumber 'D3) "Crescendo roll"))))
  (setf
   (gethash 20 remark-table) "SD3 Taiko 20in Okedo"
   (gethash 26 remark-table) "SD3 Taiko 26in Okedo Daiko")
  (sd3-instrument okedo ew-sd3-taiko keymap-table remark-table)
  (sd3-query ?okedo 'okedo remark-table))
  

