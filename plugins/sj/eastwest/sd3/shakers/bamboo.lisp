;;;; CYCO plugins sj eastwest sd3 shakers bamboo-shaker.lisp
;;;;


(param bamboo-shaker nil)
(defun bamboo-shaker (&key (parent ew-sd3-shakers)
		   channel dynamic-map remarks)
  (setf bamboo-shaker (make-instrument 'bamboo-shaker
			       :transient t
			       :parent parent
			       :channel channel
			       :dynamic-map dynamic-map
			       :remarks (or remarks "SD3 Bamboo-Shaker RR 2")
			       :keynumber-map (symbolic-keynumber-map
					       (list
						(list 'LONG-1 (ew-keynumber 'C1 ))
						(list 'LONG-2 (ew-keynumber 'D1 ))
						(list 'LONG-3 (ew-keynumber 'E1 ))
						(list 'LONG-4 (ew-keynumber 'F1 ))
						(list 'LONG-5 (ew-keynumber 'G1 ))
						(list 'LONG-6 (ew-keynumber 'A1 ))
						(list 'SHORT-1 (ew-keynumber 'B1 ))
						(list 'SHORT-2 (ew-keynumber 'C2 ))
						(list 'SHORT-3 (ew-keynumber 'D2 ))
						(list 'SHORT-4 (ew-keynumber 'E2 ))
						(list 'SHORT-5 (ew-keynumber 'F2 ))
						(list 'SHORT-PERF-01  (ew-keynumber 'G2 ))
						(list 'SHORT-PERF-02  (ew-keynumber 'A2 ))
						(list 'SHORT-PERF-03  (ew-keynumber 'B2 ))
						(list 'SHORT-PERF-04  (ew-keynumber 'C3 ))
						(list 'SHORT-PERF-05  (ew-keynumber 'D3 ))
						(list 'SHORT-PERF-06  (ew-keynumber 'E3 ))
						(list 'SHORT-PERF-07  (ew-keynumber 'F3 ))
						(list 'SHORT-PERF-08  (ew-keynumber 'G3 ))
						(list 'SHORT-PERF-09 (ew-keynumber 'A3 ))
						(list 'SHORT-PERF-10 (ew-keynumber 'E4 ))
						(list 'SHORT-PERF-11 (ew-keynumber 'G4 ))
						(list 'SHORT-PERF-12 (ew-keynumber 'A4 ))
						(list 'SHORT-PERF-13 (ew-keynumber 'B4 ))
						(list 'LONG-PERF-1 (ew-keynumber 'B3 ))
						(list 'LONG-PERF-2 (ew-keynumber 'C4 ))
						(list 'LONG-PERF-3 (ew-keynumber 'D4 ))
						(list 'LONG-PERF-4 (ew-keynumber 'F4 )))))))
