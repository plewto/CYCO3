;;;; CYCO plugins sj eastwest bigdrums miscellaneous.lisp
;;;; lord-of-toms
;;;; rosebowl
;;;; dohn
;;;; surdos
;;;; boule-drum
;;;; double-ceramic-drum
;;;; double-headed-tree-drum

(param lord-of-toms nil)
(defun lord-of-toms (&key (parent ew-sd3-bigdrums)
			  channel dynamic-map remarks)
  (setf lord-of-toms (make-instrument 'lord-of-toms
				      :parent parent
				      :transient t
				      :channel channel
				      :dynamic-map dynamic-map
				      :remarks (or remarks "SD3 BigDrums lord-of-toms")
				      :keynumber-map (circular-list-keynumber-map
						      (white-keys
						       (ew-keynumber 'c1)
						       (ew-keynumber 'd2))))))

(param rosebowl nil)
(defun rosebowl (&key (parent ew-sd3-bigdrums)
		      channel dynamic-map remarks)
  (cyco-warning "Stormdrum3 10-foot RoseBowl drum produces very low frequencies ~ Use with caution")
  (setf rosebowl (make-instrument 'rosebowl
				  :parent parent
				  :transient t
				  :channel channel
				  :dynamic-map dynamic-map
				  :remarks (or remarks "SD3 Remo Rose Bowl 10-foot *** CAUTION SUBSONICS ***")
				  :keynumber-map (circular-list-keynumber-map
						  (white-keys (ew-keynumber 'c1)
							      (ew-keynumber 'b2))))))
(param dohn nil)
(defun dohn (&key (parent ew-sd3-bigdrums)
		  channel dynamic-map remarks)
  (setf dohn (make-instrument 'dohn
			      :transient t
			      :parent parent
			      :channel channel
			      :dynamic-map dynamic-map
			      :remarks (or remarks "SD3 Big Drums   Punjabi Dohn")
			      :keynumber-map (symbolic-keynumber-map
					      (list (list 'LOW-CENTER (ew-keynumber 'C1 ))
						    (list 'LOW-FLAM1  (ew-keynumber 'D1 ))
						    (list 'LOW-FLAM2  (ew-keynumber 'E1 ))
						    (list 'LOW-FLAM3  (ew-keynumber 'F1 ))
						    (list 'LOW-RIM    (ew-keynumber 'G1 ))
						    (list 'LOW-CENTER-ROLL  (ew-keynumber 'A1 ))
						    (list 'HIGH-CENTER-ROLL (ew-keynumber 'B1 ))
						    (list 'HIGH-CENTER (ew-keynumber 'C3 ))
						    (list 'HIGH-RIM1   (ew-keynumber 'D3 ))
						    (list 'HIGH-MULTIHIT1 (ew-keynumber 'E3 ))
						    (list 'HIGH-MULTIHIT2 (ew-keynumber 'F3 ))
						    (list 'HIGH-MULTIHIT3 (ew-keynumber 'G3 ))
						    (list 'HIGH-RIM2  (ew-keynumber 'A3 ))
						    (list 'HIGH-RIM3  (ew-keynumber 'B3 ))
						    (list 'HIGH-EDGE  (ew-keynumber 'C4 ))
						    (list 'HIGH-EDGE2 (ew-keynumber 'D4 )))))))

(param surdos nil)
(defun surdos (&key (parent ew-sd3-bigdrums)
		  channel dynamic-map remarks)
  (setf surdos (make-instrument 'surdos
			      :transient t
			      :parent parent
			      :channel channel
			      :dynamic-map dynamic-map
			      :remarks (or remarks "SD3 Big Drums   Surdos")
			      :keynumber-map (symbolic-keynumber-map
					      (list (list '22-OPEN  (ew-keynumber 'C1 ))
						    (list '22-MUTE1 (ew-keynumber 'D1 ))
						    (list '22-MUTE2 (ew-keynumber 'E1 ))
						    (list '22-RIM1  (ew-keynumber 'F1 ))
						    (list '22-RIM2  (ew-keynumber 'G1 ))
						    (list '20-OPEN  (ew-keynumber 'C2 ))
						    (list '20-MUTE  (ew-keynumber 'D2 ))
						    (list '20-RIM   (ew-keynumber 'E2 ))
						    (list '16-OPEN  (ew-keynumber 'C3 ))
						    (list '16-MUTE1 (ew-keynumber 'D3 ))
						    (list '16-MUTE2 (ew-keynumber 'E3 ))
						    (list '16-RIM   (ew-keynumber 'F3 )))))))

(param boule-drum nil)
(defun boule-drum (&key (parent ew-sd3-bigdrums)
		  channel dynamic-map remarks)
  (setf boule-drum (make-instrument 'boule-drum
			      :transient t
			      :parent parent
			      :channel channel
			      :dynamic-map dynamic-map
			      :remarks (or remarks "SD3 Big Drums   Boule Custom")
			      :keynumber-map (symbolic-keynumber-map
					      (list (list 'BASS      (ew-keynumber 'C1 ))
						    (list 'BASS-FLAM (ew-keynumber 'D1 ))
						    (list 'CENTER-1  (ew-keynumber 'E1 ))
						    (list 'CENTER-2  (ew-keynumber 'F1 ))
						    (list 'EDGE      (ew-keynumber 'G1 ))
						    (list 'RIM       (ew-keynumber 'A1 ))
						    (list 'RIM-FLAN  (ew-keynumber 'B1 ))
						    (list 'PERF-1    (ew-keynumber 'C2 ))
						    (list 'PERF-2    (ew-keynumber 'E2 ))
						    (list 'PERF-3    (ew-keynumber 'F2 ))
						    (list 'PERF-4    (ew-keynumber 'G2 )))))))

(param double-ceramic-drum nil)
(defun double-ceramic-drum (&key (parent ew-sd3-bigdrums)
		  channel dynamic-map remarks)
  (setf double-ceramic-drum (make-instrument 'double-ceramic-drum
			      :transient t
			      :parent parent
			      :channel channel
			      :dynamic-map dynamic-map
			      :remarks (or remarks "SD3 Big Drums  Double Ceramic Drum")
			      :keynumber-map (symbolic-keynumber-map
					      (list
					       (list 'OPEN-RH (ew-keynumber 'C1 ))
					       (list 'MUTE-DOUBLE (ew-keynumber 'D1 ))
					       (list 'OPEN-EDGE-RH (ew-keynumber 'E1 ))
					       (list 'RIM-RH (ew-keynumber 'F1 ))
					       (list 'OPEN-LH (ew-keynumber 'G1 ))
					       (list 'RIM-LH (ew-keynumber 'A1 ))
					       (list 'OPEN-EDGE-RH (ew-keynumber 'B1 ))
					       (list 'ROLLS-1 (ew-keynumber 'C2 ))
					       (list 'ROLLS-2 (ew-keynumber 'D2 ))
					       (list 'ROLLS-3 (ew-keynumber 'E2 ))
					       (list 'ROLLS-4 (ew-keynumber 'F2 ))
					       (list 'ROLLS-5 (ew-keynumber 'G2 ))
					       (list 'ROLLS-6 (ew-keynumber 'A2 ))
					       (list 'LONG-ROLLS-1 (ew-keynumber 'B2 ))
					       (list 'LONG-ROLLS-2 (ew-keynumber 'C3 ))
					       (list 'LONG-ROLLS-3 (ew-keynumber 'D3 ))
					       (list 'LONG-ROLLS-4 (ew-keynumber 'E3 ))
					       (list 'LONG-ROLLS-5 (ew-keynumber 'F3 ))
					       (list 'LONG-ROLLS-6 (ew-keynumber 'G3 ))
					       (list 'PERF-01 (ew-keynumber 'D4 ))
					       (list 'PERF-02 (ew-keynumber 'E4 ))
					       (list 'PERF-03 (ew-keynumber 'F4 ))
					       (list 'PERF-04 (ew-keynumber 'G4 ))
					       (list 'PERF-05 (ew-keynumber 'A4 ))
					       (list 'PERF-06 (ew-keynumber 'B4 ))
					       (list 'PERF-07 (ew-keynumber 'C5 ))
					       (list 'PERF-08 (ew-keynumber 'D5 ))
					       (list 'PERF-09 (ew-keynumber 'E5 ))
					       (list 'PERF-10 (ew-keynumber 'F5 ))
					       (list 'PERF-11 (ew-keynumber 'G5 ))
					       (list 'PERF-12 (ew-keynumber 'A5 ))
					       (list 'PERF-13 (ew-keynumber 'B5 ))
					       (list 'PERF-14 (ew-keynumber 'C6 ))
					       (list 'PERF-15 (ew-keynumber 'D6 ))
					       (list 'PERF-16 (ew-keynumber 'E6 ))
					       (list 'PERF-17 (ew-keynumber 'F6 ))
					       (list 'PERF-18 (ew-keynumber 'G6 ))
					       (list 'PERF-19 (ew-keynumber 'A6 ))))))) 



(param double-headed-tree-drum nil)
(defun double-headed-tree-drum (&key (parent ew-sd3-bigdrums)
		  channel dynamic-map remarks)
  (setf double-headed-tree-drum (make-instrument 'double-headed-tree-drum
			      :transient t
			      :parent parent
			      :channel channel
			      :dynamic-map dynamic-map
			      :remarks (or remarks "SD3 Big Drums  Double Headed Tree Drum")
			      :keynumber-map (symbolic-keynumber-map
					      (list
					       (list 'OPEN-1   (ew-keynumber 'C1 ))
					       (list 'OPEN-2   (ew-keynumber 'D1 ))
					       (list 'MUTE-1   (ew-keynumber 'E1 ))
					       (list 'MUTE-2   (ew-keynumber 'F1 ))
					       (list 'RIM-1    (ew-keynumber 'G1 ))
					       (list 'RIM-2    (ew-keynumber 'A1 ))
					       (list 'FLAM-1   (ew-keynumber 'B1 ))
					       (list 'FLAM-2   (ew-keynumber 'C2 ))
					       (list 'ROLLS-01 (ew-keynumber 'D2 ))
					       (list 'ROLLS-02 (ew-keynumber 'E2 ))
					       (list 'ROLLS-03 (ew-keynumber 'F2 ))
					       (list 'ROLLS-04 (ew-keynumber 'G2 ))
					       (list 'ROLLS-05 (ew-keynumber 'A2 ))
					       (list 'ROLLS-06 (ew-keynumber 'B2 ))
					       (list 'ROLLS-07 (ew-keynumber 'C3 ))
					       (list 'ROLLS-08 (ew-keynumber 'D3 ))
					       (list 'ROLLS-09 (ew-keynumber 'E3 ))
					       (list 'ROLLS-10 (ew-keynumber 'F3 ))
					       (list 'ROLLS-11 (ew-keynumber 'G3 ))
					       (list 'ROLLS-12 (ew-keynumber 'A3 ))
					       (list 'ROLLS-13 (ew-keynumber 'B3 ))
					       (list 'ROLLS-14 (ew-keynumber 'C4 ))
					       (list 'ROLLS-15 (ew-keynumber 'D4 ))
					       (list 'ROLLS-16 (ew-keynumber 'E4 ))
					       (list 'ROLLS-17 (ew-keynumber 'F4 ))
					       (list 'PERF-1   (ew-keynumber 'G4 ))
					       (list 'PERF-2   (ew-keynumber 'A4 ))
					       (list 'PERF-3   (ew-keynumber 'B4 ))
					       (list 'PERF-4   (ew-keynumber 'C5 )))))))
						 
