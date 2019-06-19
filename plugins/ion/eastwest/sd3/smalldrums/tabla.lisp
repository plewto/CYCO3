;;;; CYCO plugins sj eastwest sd3 smalldrums tabla.lisp
;;;;

(param tabla nil)

(defun tabla (&key (parent ew-sd3-smalldrums)
		       channel
		       dynamic-map
		       (remarks "SD3 Small Drums Tabla Guru"))
  (setf tabla (make-instrument 'tabla
			       :transient t
			       :parent parent
			       :channel channel
			       :dynamic-map dynamic-map
			       :remarks remarks
			       :keynumber-map (symbolic-keynumber-map
					       (list
						(list 'E-OPEN (ew-keynumber 'C1 ))
						(list 'E-MUTE (ew-keynumber 'D1 ))
						(list 'E-SLAP-BEND (ew-keynumber 'E1 ))
						(list 'E-RIM (ew-keynumber 'F1 ))
						(list 'E-RIM-DOUBLE (ew-keynumber 'G1 ))
						(list 'E-DOUBLE (ew-keynumber 'A1 ))
						(list 'E-MUTE-SLAP (ew-keynumber 'B1 ))
						(list 'EE (ew-keynumber 'C2 ))
						(list 'A-OPEN (ew-keynumber 'D2 ))
						(list 'A-RIM (ew-keynumber 'E2 ))
						(list 'A-RIM-HARMONIC (ew-keynumber 'F2 ))
						(list 'A-RIM-DOUBLE (ew-keynumber 'G2 ))
						(list 'A-CENTER-DOUBLE (ew-keynumber 'A2 ))
						(list 'A-CENTER-MUTE (ew-keynumber 'B2 ))
						(list 'A-RIM-MUTE (ew-keynumber 'C3 ))
						(list 'SWITCH-01 (ew-keynumber 'D3 ))
						(list 'SWITCH-02 (ew-keynumber 'E3 ))
						(list 'SWITCH-03 (ew-keynumber 'F3 ))
						(list 'SWITCH-04 (ew-keynumber 'G3 ))
						(list 'SWITCH-05 (ew-keynumber 'A3 ))
						(list 'SWITCH-06 (ew-keynumber 'B3 ))
						(list 'SWITCH-07 (ew-keynumber 'C4 ))
						(list 'SWITCH-08 (ew-keynumber 'D4 ))
						(list 'CS-OPEN (ew-keynumber 'E4 ))
						(list 'CS-DOUBLE (ew-keynumber 'F4 ))
						(list 'CS-RIM (ew-keynumber 'G4 ))
						(list 'CS-RIM-HARMONIC (ew-keynumber 'A4 ))
						(list 'CS-RIM-DOUBLE (ew-keynumber 'B4 ))
						(list 'CS-RIM-MUTE (ew-keynumber 'C5 ))
						(list 'CS-CENTER-MUTE (ew-keynumber 'D5 ))
						(list 'CS-CENTER-MUTE-DOUBLE (ew-keynumber 'E5 ))
						(list 'SWITCH-09 (ew-keynumber 'F5 ))
						(list 'SWITCH-10 (ew-keynumber 'G5 ))
						(list 'SWITCH-11 (ew-keynumber 'A5 ))
						(list 'SWITCH-11 (ew-keynumber 'B5 ))
						(list 'SWITCH-12 (ew-keynumber 'C6 ))
						(list 'SWITCH-13 (ew-keynumber 'D6 ))
						(list 'SWITCH-14 (ew-keynumber 'E6 ))))))) 
