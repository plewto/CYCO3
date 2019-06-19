;;;; CYCO plugins sj eastwest sd3 smalldrums smalldrums.lisp
;;;;
;;;; ew-sd3-smalldrums
;;;;  |
;;;;  +-- murdungham
;;;;  +-- cohongas
;;;;  +-- damarue
;;;;  +-- gourd-drum
;;;;  +-- new-guinea-drum
;;;;  +-- ngara
;;;;  +-- olympic-min-drums (semi-tuned)
;;;;  +-- phakajaj
;;;;  +-- quijada
;;;;  +-- kohl
;;;;  +-- bongo (bauer tri-cuba)
;;;;  +-- cuica (1 2)
;;;;  +-- madal (mickey nepal generic)
;;;;  +-- tabla
;;;;  +-- tantan
;;;;  +-- tarang
;;;;  +-- udu (indian, ghana ghana-rr pakistan) **MISSING** indian-rr
;;;; (+-- talking-drum) ***MISSING***


(param murdungham nil)
(defun murdungham (&key (parent ew-sd3-smalldrums)
			channel dynamic-map remarks)
  (setf murdungham (make-instrument 'murdungham
				    :transient t
				    :parent parent
				    :channel channel
				    :dynamic-map dynamic-map
				    :remarks (or remarks "SD3 Small Drums Balinese Murdungham")
				    :keynumber-map (symbolic-keynumber-map
						    (list (list 'BASS   (ew-keynumber 'C1 ))
							  (list 'RIM    (ew-keynumber 'D1 ))
							  (list 'EDGE   (ew-keynumber 'E1 ))
							  (list 'SLAP   (ew-keynumber 'F1 ))
							  (list 'DOUBLE (ew-keynumber 'G1 ))
							  (list 'MUTE   (ew-keynumber 'A1 ))
							  (list 'ROLL   (ew-keynumber 'B1 )))))))

(param cohongas nil)
(defun cohongas (&key (parent ew-sd3-smalldrums)
			channel dynamic-map remarks)
  (setf cohongas (make-instrument 'cohongas
				    :transient t
				    :parent parent
				    :channel channel
				    :dynamic-map dynamic-map
				    :remarks (or remarks "SD3 Small Drums Cohongas  (130 BPM)")
				    :keynumber-map (symbolic-keynumber-map
						    (list
						     (list 'LOW-OPEN   (ew-keynumber 'C1 ))
						     (list 'LOW-SLAP   (ew-keynumber 'D1 ))
						     (list 'LOW-MUTE   (ew-keynumber 'E1 ))
						     (list 'MID-OPEN   (ew-keynumber 'F1 ))
						     (list 'MID-SLAP   (ew-keynumber 'G1 ))
						     (list 'MID-MUTE   (ew-keynumber 'A1 ))
						     (list 'HIGH-OPEN  (ew-keynumber 'B1 ))
						     (list 'HIGH-SLAP  (ew-keynumber 'C2 ))
						     (list 'HIGH-MUTE  (ew-keynumber 'D2 ))
						     (list 'LOW-ROLL-1 (ew-keynumber 'E2 ))
						     (list 'LOW-ROLL-2 (ew-keynumber 'F2 ))
						     (list 'LOW-ROLL-3 (ew-keynumber 'G2 ))
						     (list 'MID-ROLL-1 (ew-keynumber 'A2 ))
						     (list 'MID-ROLL-2 (ew-keynumber 'B2 ))
						     (list 'MID-ROLL-3 (ew-keynumber 'C3 ))
						     (list 'HIGH-ROLL-1 (ew-keynumber 'D3 ))
						     (list 'HIGH-ROLL-2 (ew-keynumber 'E3 ))
						     (list 'HIGH-ROLL-3 (ew-keynumber 'F3 ))
						     (list 'fx-1   (ew-keynumber 'G3 ))
						     (list 'fx-2   (ew-keynumber 'A3 ))
						     (list 'fx-3   (ew-keynumber 'B3 ))
						     (list 'fx-4   (ew-keynumber 'C4 ))
						     (list 'fx-5   (ew-keynumber 'D4 ))
						     (list 'fx-6   (ew-keynumber 'E4 ))
						     (list 'fx-7   (ew-keynumber 'F4 ))
						     (list 'fx-8   (ew-keynumber 'G4 ))
						     (list 'perf-1 (ew-keynumber 'A4 ))
						     (list 'perf-2 (ew-keynumber 'B4 ))
						     (list 'perf-3 (ew-keynumber 'C5 ))
						     (list 'perf-4 (ew-keynumber 'D5 ))
						     (list 'perf-5 (ew-keynumber 'E5 ))
						     (list 'perf-6 (ew-keynumber 'F5 ))
						     (list 'perf-7 (ew-keynumber 'G5 ))
						     (list 'perf-8 (ew-keynumber 'A5 ))
						     (list 'perf-9 (ew-keynumber 'B5 ))))))) 
						     
(param damarue nil)
(defun damarue (&key (parent ew-sd3-smalldrums)
		     channel dynamic-map remarks)
  (setf damarue (make-instrument 'damarue
				 :transient t
				 :parent parent
				 :channel channel
				 :dynamic-map dynamic-map
				 :remarks (or remarks "SD3 Small Drums Damarue FX")
				 :keynumber-map (circular-list-keynumber-map
						 (white-keys (ew-keynumber 'c1)
							     (ew-keynumber 'f4))))))

(param gourd-drum nil)
(defun gourd-drum (&key (parent ew-sd3-smalldrums)
			channel dynamic-map remarks)
  (setf gourd-drum (make-instrument 'gourd-drum
				    :transient t
				    :parent parent
				    :channel channel
				    :dynamic-map dynamic-map
				    :remarks (or remarks "SD3 Small Drums Gourd Drum (140/149 BPM)")
				    :keynumber-map (symbolic-keynumber-map
						    (list
						     (list 'LOW-1 (ew-keynumber 'C1 ))
						     (list 'LOW-2 (ew-keynumber 'D1 ))
						     (list 'LOW-3 (ew-keynumber 'E1 ))
						     (list 'LOW-4 (ew-keynumber 'F1 ))
						     (list 'LOW-5 (ew-keynumber 'G1 ))
						     (list 'LOW-6 (ew-keynumber 'A1 ))
						     (list 'HIGH-1 (ew-keynumber 'B1 ))
						     (list 'HIGH-2 (ew-keynumber 'C2 ))
						     (list 'HIGH-3 (ew-keynumber 'D2 ))
						     (list 'HIGH-4 (ew-keynumber 'E2 ))
						     (list 'HIGH-5 (ew-keynumber 'F2 ))
						     (list 'HIGH-6 (ew-keynumber 'G2 ))
						     (list 'PERF-140-01 (ew-keynumber 'A2 ))
						     (list 'PERF-140-02 (ew-keynumber 'B2 ))
						     (list 'PERF-140-03 (ew-keynumber 'C3 ))
						     (list 'PERF-140-04 (ew-keynumber 'D3 ))
						     (list 'PERF-140-05 (ew-keynumber 'E3 ))
						     (list 'PERF-140-06 (ew-keynumber 'F3 ))
						     (list 'PERF-140-07 (ew-keynumber 'G3 ))
						     (list 'PERF-140-08 (ew-keynumber 'A3 ))
						     (list 'PERF-140-09 (ew-keynumber 'B3 ))
						     (list 'PERF-140-10 (ew-keynumber 'C4 ))
						     (list 'PERF-140-11 (ew-keynumber 'D4 ))
						     (list 'PERF-140-12 (ew-keynumber 'E4 ))
						     (list 'PERF-140-13 (ew-keynumber 'F4 ))
						     (list 'PERF-140-14 (ew-keynumber 'G4 ))
						     (list 'PERF-140-15 (ew-keynumber 'A4 ))
						     (list 'PERF-140-16 (ew-keynumber 'B4 ))
						     (list 'PERF-149-01 (ew-keynumber 'C5  ))
						     (list 'PERF-149-02 (ew-keynumber 'CS5 ))
						     (list 'PERF-149-03 (ew-keynumber 'DS5 ))
						     (list 'PERF-149-04 (ew-keynumber 'F5  ))
						     (list 'PERF-149-05 (ew-keynumber 'FS5 ))
						     (list 'PERF-149-06 (ew-keynumber 'GS5 ))
						     (list 'PERF-149-07 (ew-keynumber 'AS5 ))
						     (list 'PERF-149-08 (ew-keynumber 'C6  ))
						     (list 'PERF-149-09 (ew-keynumber 'CS6 ))
						     (list 'PERF-149-10 (ew-keynumber 'DS6 )))))))
						     

(param new-guinea-drum nil)
(defun new-guinea-drum (&key (parent ew-sd3-smalldrums)
			     channel dynamic-map remarks)
  (setf new-guinea-drum (make-instrument 'new-guinea-drum
					 :transient t
					 :parent parent
					 :channel channel
					 :dynamic-map dynamic-map
					 :remarks (or remarks "SD3 Small Drums  New Guinea Drum"
						      :keynumber-map (circular-list-keynumber-map
								      (white-keys (ew-keynumber 'c1)
										  (ew-keynumber 'f3)))))))

(param ngara nil)
(defun ngara (&key (parent ew-sd3-smalldrums)
			     channel dynamic-map remarks)
  (setf ngara (make-instrument 'ngara
			       :transient t
			       :parent parent
			       :channel channel
			       :dynamic-map dynamic-map
			       :remarks (or remarks "SD3 Small Drums  Ngara")
					    :keynumber-map (symbolic-keynumber-map
							    (list
							     (list 'LOW-BASS    (ew-keynumber 'C1 ))
							     (list 'LOW-CENTER  (ew-keynumber 'D1 ))
							     (list 'LOW-EDGE    (ew-keynumber 'E1 ))
							     (list 'HIGH-BASS   (ew-keynumber 'F1 ))
							     (list 'HIGH-CENTER (ew-keynumber 'G1 ))
							     (list 'HIGH-EDGE   (ew-keynumber 'A1 ))
							     (list 'HIGH-CENTER-2 (ew-keynumber 'B1 ))
							     (list 'HIGH-FLAM   (ew-keynumber 'C2 ))
							     (list 'HIGH-HIT-1  (ew-keynumber 'D2 ))
							     (list 'HIGH-HIT-2  (ew-keynumber 'E2 ))
							     (list 'ROLL-1 (ew-keynumber 'F2 ))
							     (list 'ROLL-2 (ew-keynumber 'G2 ))
							     (list 'ROLL-3 (ew-keynumber 'A2 ))
							     (list 'ROLL-4 (ew-keynumber 'B2 ))
							     (list 'ROLL-5 (ew-keynumber 'C3 ))
							     (list 'ROLL-6 (ew-keynumber 'D3 ))
							     (list 'ROLL-7 (ew-keynumber 'E3 ))
							     (list 'ROLL-8 (ew-keynumber 'F3 )))))))

(param olympic-mini-drums nil)
(defun olympic-mini-drums (&key (parent ew-sd3-smalldrums)
			     channel dynamic-map remarks)
  (setf olympic-mini-drums (make-instrument 'olympic-mini-drums
			       :transient t
			       :parent parent
			       :channel channel
			       :dynamic-map dynamic-map
			       :remarks (or remarks "SD3 Small Drums  Olympic-Mini-Drums (semi tuned)")
					    :keynumber-map (symbolic-keynumber-map
							    (list
							     (list 'OPEN-A (ew-keynumber 'C1 ))
							     (list 'OPEN-B (ew-keynumber 'D1 ))
							     (list 'OPEN-C (ew-keynumber 'E1 ))
							     (list 'OPEN-D (ew-keynumber 'F1 ))
							     (list 'OPEN-E (ew-keynumber 'G1 ))
							     (list 'ROLL-A (ew-keynumber 'C2 ))
							     (list 'ROLL-B (ew-keynumber 'D2 ))
							     (list 'ROLL-C (ew-keynumber 'E2 ))
							     (list 'ROLL-D (ew-keynumber 'F2 ))
							     (list 'ROLL-E (ew-keynumber 'G2 ))
							     (list 'PERF-A (ew-keynumber 'C3 ))
							     (list 'PERF-B (ew-keynumber 'D3 ))
							     (list 'PERF-C (ew-keynumber 'E3 ))
							     (list 'PERF-D (ew-keynumber 'F3 ))
							     (list 'PERF-E (ew-keynumber 'G3 )))))))

(param phakajaj nil)
(defun phakajaj (&key (parent ew-sd3-smalldrums)
			     channel dynamic-map remarks)
  (setf phakajaj (make-instrument 'phakajaj
			       :transient t
			       :parent parent
			       :channel channel
			       :dynamic-map dynamic-map
			       :remarks (or remarks "SD3 Small Drums  Phakajaj")
			       :keynumber-map (circular-list-keynumber-map
					       (white-keys (ew-keynumber 'c1)
							   (ew-keynumber 'e2))))))

(param quijada nil)
(defun quijada (&key (parent ew-sd3-smalldrums)
			     channel dynamic-map remarks)
  (setf quijada (make-instrument 'quijada
			       :transient t
			       :parent parent
			       :channel channel
			       :dynamic-map dynamic-map
			       :remarks (or remarks "SD3 Small Drums  Quijada")
			       :keynumber-map (circular-list-keynumber-map
					       (white-keys (ew-keynumber 'c1)
							   (ew-keynumber 'c5))))))

(param kohl nil)
(defun kohl (&key (parent ew-sd3-smalldrums)
			     channel dynamic-map remarks)
  (setf kohl (make-instrument 'kohl
			       :transient t
			       :parent parent
			       :channel channel
			       :dynamic-map dynamic-map
			       :remarks (or remarks "SD3 Small Drums  South Indian Kohl  (120 BPM)")
			       :keynumber-map (symbolic-keynumber-map
					       (list
						(list 'BASS      (ew-keynumber 'C1 ))
						(list 'RIM       (ew-keynumber 'D1 ))
						(list 'DOUBLE-1  (ew-keynumber 'E1 ))
						(list 'DOUBLE-2  (ew-keynumber 'F1 ))
						(list 'OPEN-RH-1 (ew-keynumber 'G1 ))
						(list 'OPEN-RH-2 (ew-keynumber 'A1 ))
						(list 'SLAP      (ew-keynumber 'B1 ))
						(list 'MUTE      (ew-keynumber 'C2 ))
						(list 'ROLL-1 (ew-keynumber 'D2 ))
						(list 'ROLL-2 (ew-keynumber 'E2 ))
						(list 'ROLL-3 (ew-keynumber 'F2 ))
						(list 'ROLL-4 (ew-keynumber 'G2 ))
						(list 'ROLL-5 (ew-keynumber 'A2 ))
						(list 'ROLL-6 (ew-keynumber 'B2 ))
						(list 'ROLL-7 (ew-keynumber 'C3 ))
						(list 'ROLL-8 (ew-keynumber 'D3 ))
						(list 'PERF-1 (ew-keynumber 'E3 ))
						(list 'PERF-2 (ew-keynumber 'F3 ))
						(list 'PERF-3 (ew-keynumber 'G3 ))
						(list 'PERF-4 (ew-keynumber 'A3 ))
						(list 'PERF-5 (ew-keynumber 'B3 ))
						(list 'PERF-6 (ew-keynumber 'C4 ))
						(list 'PERF-7 (ew-keynumber 'D4 ))
						(list 'PERF-8 (ew-keynumber 'E4 ))
						(list 'MUTE-FINGER-01 (ew-keynumber 'C7 ))
						(list 'MUTE-FINGER-02 (ew-keynumber 'D7 ))
						(list 'MUTE-FINGER-03 (ew-keynumber 'E7 ))
						(list 'MUTE-FINGER-04 (ew-keynumber 'F7 ))
						(list 'MUTE-FINGER-05 (ew-keynumber 'G7 ))
						(list 'MUTE-FINGER-06 (ew-keynumber 'A7 ))
						(list 'MUTE-FINGER-07 (ew-keynumber 'B7 ))
						(list 'MUTE-FINGER-08 (ew-keynumber 'C8 ))
						(list 'MUTE-FINGER-09 (ew-keynumber 'D8 ))
						(list 'MUTE-FINGER-10 (ew-keynumber 'E8 ))
						(list 'MUTE-FINGER-11 (ew-keynumber 'F8 )))))))


(load-plugin-file "eastwest/sd3/smalldrums/bongos")
(load-plugin-file "eastwest/sd3/smalldrums/cuica")
(load-plugin-file "eastwest/sd3/smalldrums/madal")
(load-plugin-file "eastwest/sd3/smalldrums/rapanique")
(load-plugin-file "eastwest/sd3/smalldrums/tabla")
;;(load-plugin-file "eastwest/sd3/smalldrums/talking")
(load-plugin-file "eastwest/sd3/smalldrums/tantan")
(load-plugin-file "eastwest/sd3/smalldrums/tarang")
(load-plugin-file "eastwest/sd3/smalldrums/udu")
