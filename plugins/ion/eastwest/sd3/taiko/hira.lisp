;;;; CYCO plugins sj eastwest sd3 taiko hira.lisp
;;;;

(param hira nil)

(let ((keymap-table (make-hash-table))
      (remark-table (make-hash-table)))
  (setf (gethash 44 keymap-table)(symbolic-keynumber-map
				  (list
				   (list 'CENTER-LH     (ew-keynumber 'C1 ))
				   (list 'CENTER-RH     (ew-keynumber 'D1 ))
				   (list 'CENTER-FLAM   (ew-keynumber 'E1 ))
				   (list 'OFF-CENTER-LH (ew-keynumber 'F1 ))
				   (list 'OFF-CENTER-RH (ew-keynumber 'G1 ))
				   (list 'OFF-CENTER-FLAM (ew-keynumber 'A1 ))
				   (list 'RIM-LH          (ew-keynumber 'B1 ))
				   (list 'RIM-RH          (ew-keynumber 'C2 ))
				   (list 'RIM-FLAM        (ew-keynumber 'D2 ))
				   (list 'RIM-FLAM-DOUBLE (ew-keynumber 'E2 ))
				   (list 'RIM-ROLL (ew-keynumber 'F2 ))
				   (list 'ROLL-01  (ew-keynumber 'G2 ))
				   (list 'ROLL-02  (ew-keynumber 'A2 ))
				   (list 'ROLL-03  (ew-keynumber 'B2 ))
				   (list 'ROLL-04  (ew-keynumber 'C3 ))
				   (list 'ROLL-05  (ew-keynumber 'D3 ))
				   (list 'ROLL-06  (ew-keynumber 'E3 ))
				   (list 'ROLL-07  (ew-keynumber 'F3 ))
				   (list 'ROLL-08  (ew-keynumber 'G3 ))
				   (list 'ROLL-09  (ew-keynumber 'A3) "Short stick roll")
				   (list 'ROLL-10  (ew-keynumber 'B3) "Stick roll crescendo w retarded tempo")
				   (list 'ROLL-11  (ew-keynumber 'C4) "Stick roll slow crescendo")))
	(gethash 48 keymap-table)(symbolic-keynumber-map
				  (list
				   (list 'MALLET-HIT   (ew-keynumber 'C1)) 
				   (list 'EDGE         (ew-keynumber 'D1)) 
				   (list 'EDGE-FLAM    (ew-keynumber 'E1)) 
				   (list 'CENTER       (ew-keynumber 'F1)) 
				   (list 'CENTER-FLAM  (ew-keynumber 'G1)) 
				   (list 'ENSEMBLE-HIT (ew-keynumber 'A1)) 
				   (list 'ENSEMBLE-RIM (ew-keynumber 'B1)) 
				   (list 'STICK-RIM    (ew-keynumber 'C2)) 
				   (list 'ROLL-01 (ew-keynumber 'D2) "Long mallet roll")
				   (list 'ROLL-02 (ew-keynumber 'E2) "Long stick roll")
				   (list 'ROLL-03 (ew-keynumber 'F2) "Decrescendo")
				   (list 'ROLL-04 (ew-keynumber 'G2) "Decrescendo")
				   (list 'ROLL-05 (ew-keynumber 'A2) "Mallet crescendo")
				   (list 'ROLL-06 (ew-keynumber 'B2) "Mallet crescendo")
				   (list 'ROLL-07 (ew-keynumber 'C3) "Mallet crescendo")
				   (list 'ROLL-08 (ew-keynumber 'D3) "Short crescendo roll")
				   (list 'ROLL-09 (ew-keynumber 'E3) "Short crescendo roll")
				   (list 'ROLL-10 (ew-keynumber 'F3) "Short crescendo roll")
				   (list 'ROLL-11 (ew-keynumber 'G3) "Short crescendo roll")
				   (list 'ROLL-12 (ew-keynumber 'A3) "Long crescendo roll")
				   (list 'ROLL-13 (ew-keynumber 'B3) "Long crescendo roll")
				   (list 'ROLL-14 (ew-keynumber 'C4) "Long crescendo roll")
				   (list 'ROLL-15 (ew-keynumber 'D4) "Decrescendo")
				   (list 'ROLL-16 (ew-keynumber 'E4) "Mallet crescendo")
				   (list 'ROLL-17 (ew-keynumber 'F4) "Mallet crescendo")
				   (list 'ROLL-18 (ew-keynumber 'D4) "Long crescendo roll")
				   (list 'ROLL-19 (ew-keynumber 'E4) "Long crescendo roll")
				   (list 'ROLL-20 (ew-keynumber 'F4) "Long crescendo roll")
				   (list 'ROLL-21 (ew-keynumber 'G4) "Long crescendo roll")
				   (list 'ROLL-22 (ew-keynumber 'A4) "Long crescendo roll")
				   (list 'ROLL-23 (ew-keynumber 'B4) "Long crescendo roll")
				   (list 'ROLL-24 (ew-keynumber 'C5) "Long crescendo roll")
				   (list 'ROLL-25 (ew-keynumber 'D5) "Long crescendo roll")
				   (list 'ROLL-26 (ew-keynumber 'E5) "Long crescendo roll")
				   (list 'ROLL-27 (ew-keynumber 'F5) "Long crescendo roll")
				   (list 'ROLL-28 (ew-keynumber 'G5) "Long crescendo roll")
				   (list 'SCRAPE-1 (ew-keynumber 'G4)) 
				   (list 'SCRAPE-2 (ew-keynumber 'A4)) 
				   (list 'SCRAPE-3 (ew-keynumber 'B4)) 
				   (list 'SCRAPE-4 (ew-keynumber 'C5))))
	(gethash 60 keymap-table)(symbolic-keynumber-map
				  (list
				   (list 'MALLET-HIT   (ew-keynumber 'C1)) 
				   (list 'EDGE         (ew-keynumber 'D1)) 
				   (list 'CENTER       (ew-keynumber 'E1)) 
				   (list 'CENTER-FLAM  (ew-keynumber 'F1)) 
				   (list 'EDGE-FLAM    (ew-keynumber 'G1)) 
				   (list 'ENSEMBLE-HIT (ew-keynumber 'A1)) 
				   (list 'ENSEMBLE-RIM (ew-keynumber 'B1)) 
				   (list 'STICK-RIM    (ew-keynumber 'C2))
				   (list 'ROLL-01 (ew-keynumber 'D2) "Long mallet roll")
				   (list 'ROLL-02 (ew-keynumber 'E2) "Long stick roll")
				   (list 'ROLL-03 (ew-keynumber 'F2) "Decrescendo")
				   (list 'ROLL-04 (ew-keynumber 'G2) "Decrescendo")
				   (list 'ROLL-05 (ew-keynumber 'A2) "Long mallet crescendo")
				   (list 'ROLL-06 (ew-keynumber 'B2) "Short mallet crescendo")
				   (list 'ROLL-07 (ew-keynumber 'C3) "Short mallet crescendo")
				   (list 'ROLL-08 (ew-keynumber 'D3) "Short crescendo")
				   (list 'ROLL-09 (ew-keynumber 'E3) "Short crescendo")
				   (list 'ROLL-10 (ew-keynumber 'F3) "Short crescendo")
				   (list 'ROLL-11 (ew-keynumber 'G3) "Short crescendo")
				   (list 'ROLL-12 (ew-keynumber 'A3) "Long crescendo")
				   (list 'ROLL-13 (ew-keynumber 'B3) "Long crescendo")
				   (list 'ROLL-14 (ew-keynumber 'C4) "Long crescendo")
				   (list 'ROLL-15 (ew-keynumber 'D4) "Decrescendo")
				   (list 'ROLL-16 (ew-keynumber 'E4) "Long mallet crescendo")
				   (list 'ROLL-17 (ew-keynumber 'F4) "Long mallet crescendo")
				   (list 'SCRAPE-1 (ew-keynumber 'G4)) 
				   (list 'SCRAPE-2 (ew-keynumber 'A4)) 
				   (list 'SCRAPE-3 (ew-keynumber 'B4)) 
				   (list 'SCRAPE-4 (ew-keynumber 'C5)) 
				   (list 'SCRAPE-5 (ew-keynumber 'D5)) 
				   (list 'SCRAPE-6 (ew-keynumber 'E5)) 
				   (list 'SCRAPE-7 (ew-keynumber 'F5)) 
				   (list 'SCRAPE-8 (ew-keynumber 'G5)))))
  (setf (gethash 44 remark-table) "SD3 Taiko 44in Hira Daiko"
	(gethash 48 remark-table) "SD3 Taiko 48in Hira Daiko"
	(gethash 60 remark-table) "SD3 Taiko 60in Hira Daiko")
  (sd3-instrument hira ew-sd3-taiko keymap-table remark-table)
  (sd3-query ?hira 'hira remark-table)) 
  
