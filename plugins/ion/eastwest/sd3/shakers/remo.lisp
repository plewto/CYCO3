;;;; CYCO plugins sj eastwest sd3 shakers remo-shaker.lisp
;;;;


(param remo-shaker nil)
(defun remo-shaker (&key (parent ew-sd3-shakers)
		   channel dynamic-map remarks)
  (setf remo-shaker (make-instrument 'remo-shaker
			       :transient t
			       :parent parent
			       :channel channel
			       :dynamic-map dynamic-map
			       :remarks (or remarks "SD3 Remo-Shaker RR 4")
			       :keynumber-map (symbolic-keynumber-map
					       (list
						(list 'HIT-01 (ew-keynumber 'C1 ))
						(list 'HIT-02 (ew-keynumber 'D1 ))
						(list 'HIT-03 (ew-keynumber 'E1 ))
						(list 'HIT-04 (ew-keynumber 'F1 ))
						(list 'HIT-05 (ew-keynumber 'G1 ))
						(list 'HIT-06 (ew-keynumber 'A1 ))
						(list 'HIT-07 (ew-keynumber 'B1 ))
						(list 'HIT-08 (ew-keynumber 'C2 ))
						(list 'HIT-09 (ew-keynumber 'D2 ))
						(list 'HIT-10 (ew-keynumber 'E2 ))
						(list 'HIT-11 (ew-keynumber 'F2 ))
						(list 'ROLL-1 (ew-keynumber 'G2 ))
						(list 'ROLL-2 (ew-keynumber 'A2 ))
						(list 'ROLL-3 (ew-keynumber 'B2 ))
						(list 'ROLL-4 (ew-keynumber 'C3 ))
						(list 'ROLL-5 (ew-keynumber 'D3 ))
						(list 'ROLL-6 (ew-keynumber 'E3 ))
						(list 'PERF-01 (ew-keynumber 'F3 ))
						(list 'PERF-02 (ew-keynumber 'G3 ))
						(list 'PERF-03 (ew-keynumber 'A3 ))
						(list 'PERF-04 (ew-keynumber 'B3 ))
						(list 'PERF-05 (ew-keynumber 'C4 ))
						(list 'PERF-06 (ew-keynumber 'D4 ))
						(list 'PERF-07 (ew-keynumber 'E4 ))
						(list 'PERF-08 (ew-keynumber 'F4 ))
						(list 'PERF-09 (ew-keynumber 'G4 ))
						(list 'PERF-10 (ew-keynumber 'A4 ))
						(list 'PERF-11 (ew-keynumber 'B4 ))
						(list 'PERF-12 (ew-keynumber 'C5 ))
						(list 'PERF-13 (ew-keynumber 'D5 ))
						(list 'PERF-14 (ew-keynumber 'E5 ))
						(list 'PERF-15 (ew-keynumber 'F5 )))))))
