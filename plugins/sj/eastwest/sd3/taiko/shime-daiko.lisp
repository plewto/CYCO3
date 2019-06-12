;;;; CYCO plugins sj eastwest sd3 taiko shime-daiko.lisp
;;;;

(param shime-daiko nil)

(let ((keymap-table (make-hash-table))
      (remark-table (make-hash-table)))
  (setf
   (gethash '14-16 keymap-table)(symbolic-keynumber-map
				 (list
     				  (list 'LOW-CENTER        (ew-keynumber 'C1)) 
				  (list 'LOW-CENTER-DOUBLE (ew-keynumber 'D1)) 
				  (list 'LOW-RIM           (ew-keynumber 'E1)) 
				  (list 'LOW-RIM-DOUBLE    (ew-keynumber 'F1)) 
				  (list 'LOW-RIM-FLAM-1    (ew-keynumber 'G1)) 
				  (list 'LOW-RIM-FLAM-2    (ew-keynumber 'A1)) 
				  (list 'LOW-BUZZ-1        (ew-keynumber 'B1)) 
				  (list 'LOW-BUZZ-2        (ew-keynumber 'C2)) 
				  (list 'HIGH-CENTER        (ew-keynumber 'D2)) 
				  (list 'HIGH-CENTER-DOUBLE (ew-keynumber 'E2)) 
				  (list 'HIGH-RIM           (ew-keynumber 'F2)) 
				  (list 'HIGH-RIM-DOUBLE    (ew-keynumber 'G2)) 
				  (list 'HIGH-RIM-FLAM-1    (ew-keynumber 'A2)) 
				  (list 'HIGH-RIM-FLAM-2    (ew-keynumber 'B2)) 
				  (list 'HIGH-BUZZ-1        (ew-keynumber 'C3)) 
				  (list 'HIGH-BUZZ-2        (ew-keynumber 'D3)) 
				  (list 'ENSEMBLE-LOW       (ew-keynumber 'E3)) 
				  (list 'ENSEMBLE-LOW-RIM   (ew-keynumber 'F3)) 
				  (list 'ENSEMBLE-HIGH      (ew-keynumber 'G3)) 
				  (list 'ENSEMBLE-HIGH-RIM  (ew-keynumber 'A3)) 
				  (list 'ROLL-01 (ew-keynumber 'B3) "Velocity sensitive roll") 
				  (list 'ROLL-02 (ew-keynumber 'C4) "Velocity sensitive roll") 
				  (list 'ROLL-03 (ew-keynumber 'D4) "Velocity sensitive roll") 
				  (list 'ROLL-04 (ew-keynumber 'E4) "Velocity sensitive roll") 
				  (list 'ROLL-05 (ew-keynumber 'F4) "Velocity sensitive roll") 
				  (list 'ROLL-06 (ew-keynumber 'G4) "Velocity sensitive roll") 
				  (list 'ROLL-07 (ew-keynumber 'A4) "Velocity sensitive roll") 
				  (list 'ROLL-08 (ew-keynumber 'B4) "Velocity sensitive roll") 
				  (list 'ROLL-09 (ew-keynumber 'C5) "Non-velocity sensitive roll") 
				  (list 'ROLL-10 (ew-keynumber 'D5) "Non-velocity sensitive roll") 
				  (list 'PERF-1 (ew-keynumber 'E5)) 
				  (list 'PERF-2 (ew-keynumber 'F5)))) 
   (gethash '14-remo keymap-table)(symbolic-keynumber-map
				   (list
				    (list 'CENTER-LH   (ew-keynumber 'C1)) 
				    (list 'CENTER-RH   (ew-keynumber 'D1)) 
				    (list 'CENTER-FLAM (ew-keynumber 'E1)) 
				    (list 'EDGE-LH     (ew-keynumber 'F1)) 
				    (list 'EDGE-RH     (ew-keynumber 'G1)) 
				    (list 'EDGE-FLAM   (ew-keynumber 'A1)) 
				    (list 'RIM-LH      (ew-keynumber 'B1)) 
				    (list 'RIM-RH      (ew-keynumber 'C2)) 
				    (list 'RIM-FLAM    (ew-keynumber 'D2)) 
				    (list 'ROLL-1 (ew-keynumber 'E2) "Short roll")
				    (list 'ROLL-2 (ew-keynumber 'F2) "Crescendo roll")
				    (list 'ROLL-3 (ew-keynumber 'G2) "Crescendo roll")))
   (gethash '14-japan keymap-table)(symbolic-keynumber-map
				    (list
				     (list 'CENTER-LH   (ew-keynumber 'C1)) 
				     (list 'CENTER-RH   (ew-keynumber 'D1)) 
				     (list 'CENTER-FLAM (ew-keynumber 'E1)) 
				     (list 'EDGE-LH     (ew-keynumber 'F1)) 
				     (list 'EDGE-RH     (ew-keynumber 'G1)) 
				     (list 'EDGE-FLAM   (ew-keynumber 'A1)) 
				     (list 'SHORT-ROLL  (ew-keynumber 'B1)) 
				     (list 'RIM-LH      (ew-keynumber 'C2)) 
				     (list 'RIM-RH      (ew-keynumber 'D2)) 
				     (list 'RIM-FLAM    (ew-keynumber 'E2)) 
				     (list 'ROLL-1 (ew-keynumber 'F2)) 
				     (list 'ROLL-2 (ew-keynumber 'G2)) 
				     (list 'ROLL-3 (ew-keynumber 'A2)) 
				     (list 'ROLL-4 (ew-keynumber 'B2))))
   (gethash '15 keymap-table)(symbolic-keynumber-map
			      (list
			       (list 'CENTER-LH   (ew-keynumber 'C1)) 
			       (list 'CENTER-RH   (ew-keynumber 'D1)) 
			       (list 'CENTER-FLAM (ew-keynumber 'E1)) 
			       (list 'EDGE-LH     (ew-keynumber 'F1)) 
			       (list 'EDGE-RH     (ew-keynumber 'G1)) 
			       (list 'EDGE-FLAM   (ew-keynumber 'A1)) 
			       (list 'ROLL-1 (ew-keynumber 'B1) "Short roll")
			       (list 'ROLL-2 (ew-keynumber 'C2) "Crescendo roll")
			       (list 'ROLL-3 (ew-keynumber 'D2) "Crescendo roll")))
   (gethash 'ensemble keymap-table)(circular-list-keynumber-map
				    (white-keys (ew-keynumber 'c1)
						(ew-keynumber 'e1)))
   (gethash 'metal keymap-table)(symbolic-keynumber-map
				 (list
				  (list 'CENTER-LH (ew-keynumber 'C1)) 
				  (list 'CENTER-RH (ew-keynumber 'D1)) 
				  (list 'CENTER-FLAM (ew-keynumber 'E1)) 
				  (list 'EDGE-LH (ew-keynumber 'F1)) 
				  (list 'EDGE-RH (ew-keynumber 'G1)) 
				  (list 'EDGE-FLAM (ew-keynumber 'A1)) 
				  (list 'RIM-LH (ew-keynumber 'B1)) 
				  (list 'RIM-RH (ew-keynumber 'C2)) 
				  (list 'RIM-FLAM (ew-keynumber 'D2)) 
				  (list 'ROLL-1 (ew-keynumber 'E2) "Short roll")
				  (list 'ROLL-2 (ew-keynumber 'F2) "Crescendo roll")
				  (list 'ROLL-3 (ew-keynumber 'F3) "Crescendo roll"))))
  (setf
   (gethash '14-16 remark-table) "SD3 Taiko 14in and 16in Shime Daiko Japan"
   (gethash '14-japan remark-table) "SD3 Taiko Shime Daiko 14in"
   (gethash '14-remo remark-table) "SD3 Taiko Remo Shime Daiko 14in"
   (gethash '15 remark-table) "SD3 Taiko Shime Daiko 15in Micky Custom"
   (gethash 'ensemble remark-table) "SD3 Taiko Shime Daiko Ensemble"
   (gethash 'metal remark-table) "SD3 Taiko Shime Daiko Metal")
  (sd3-instrument shime-daiko ew-sd3-taiko keymap-table remark-table)
  (sd3-query ?shime-daiko 'shime-daiko remark-table))

   
