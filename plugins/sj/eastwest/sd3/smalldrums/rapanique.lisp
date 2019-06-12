;;;; CYCO plugins sj eastwest sd3 smalldrums rapanique.lisp
;;;;

(param rapanique nil)

(defun rapanique (&key (parent ew-sd3-smalldrums)
		       channel
		       dynamic-map
		       (remarks "SD3 Small Drums Remo Rapanique"))
  (setf rapanique (make-instrument 'rapanique
				   :transient t
				   :parent parent
				   :channel channel
				   :dynamic-map dynamic-map
				   :remarks remarks
				   :keynumber-map (symbolic-keynumber-map
						   (list
						    (list 'OPEN       (ew-keynumber 'C1 ))
						    (list 'SLAP       (ew-keynumber 'D1 ))
						    (list 'OFF-CENTER (ew-keynumber 'E1 ))
						    (list 'EDGE       (ew-keynumber 'F1 ))
						    (list 'RIM-LOUD   (ew-keynumber 'G1 ))
						    (list 'RIM-SOFT   (ew-keynumber 'A1 ))
						    (list 'BUZZ-1 (ew-keynumber 'B1 ))
						    (list 'BUZZ-2 (ew-keynumber 'C2 ))
						    (list 'ROLL-1 (ew-keynumber 'D2 ))
						    (list 'ROLL-2 (ew-keynumber 'E2 ))
						    (list 'ROLL-3 (ew-keynumber 'F2 ))
						    (list 'ROLL-4 (ew-keynumber 'G2 ))
						    (list 'ROLL-5 (ew-keynumber 'A2 ))
						    (list 'PERF-01 (ew-keynumber 'B2 ))
						    (list 'PERF-02 (ew-keynumber 'C3 ))
						    (list 'PERF-03 (ew-keynumber 'D3 ))
						    (list 'PERF-04 (ew-keynumber 'E3 ))
						    (list 'PERF-05 (ew-keynumber 'F3 ))
						    (list 'PERF-06 (ew-keynumber 'G3 ))
						    (list 'PERF-07 (ew-keynumber 'A3 ))
						    (list 'PERF-08 (ew-keynumber 'B3 ))
						    (list 'PERF-09 (ew-keynumber 'C4 ))
						    (list 'PERF-10 (ew-keynumber 'D4 ))
						    (list 'PERF-11 (ew-keynumber 'E4 ))
						    (list 'PERF-12 (ew-keynumber 'F4 ))
						    (list 'PERF-13 (ew-keynumber 'G4 ))
						    (list 'PERF-14 (ew-keynumber 'A4 ))
						    (list 'PERF-15 (ew-keynumber 'B4 ))
						    (list 'PERF-16 (ew-keynumber 'C5 ))
						    (list 'PERF-17 (ew-keynumber 'D5 ))
						    (list 'PERF-18 (ew-keynumber 'E5 ))
						    (list 'PERF-19 (ew-keynumber 'F5 ))
						    (list 'PERF-20 (ew-keynumber 'G5 ))
						    (list 'PERF-21 (ew-keynumber 'A5 ))
						    (list 'PERF-22 (ew-keynumber 'B5 ))
						    (list 'PERF-23 (ew-keynumber 'C6 ))
						    (list 'PERF-24 (ew-keynumber 'D6 ))
						    (list 'PERF-25 (ew-keynumber 'E6 ))
						    (list 'PERF-26 (ew-keynumber 'F6 ))
						    (list 'PERF-27 (ew-keynumber 'G6 ))
						    (list 'PERF-28 (ew-keynumber 'A6 ))
						    (list 'PERF-29 (ew-keynumber 'B6 ))
						    (list 'PERF-30 (ew-keynumber 'C7 ))))))) 
