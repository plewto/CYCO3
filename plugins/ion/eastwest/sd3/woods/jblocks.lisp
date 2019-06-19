;;;; CYCO plugins sj eastwest SD3 woods jblocks.lisp
;;;;

(param japanese-blocks nil)

(defun japanese-blocks (&key (parent ew-sd3-woods)
			     channel dynamic-map
			     (remarks "SD3 Woods Japanese Blocks and Slapsticks"))
  (setf japanese-blocks (make-instrument 'japanese-blocks
					 :transient t
					 :remarks remarks
					 :parent parent
					 :channel channel
					 :dynamic-map dynamic-map
					 :keynumber-map (symbolic-keynumber-map
							 (list
							  (list 'SHINTO-BLOCK-1 (ew-keynumber 'C1)) 
							  (list 'SHINTO-BLOCK-2 (ew-keynumber 'D1)) 
							  (list 'SHINTO-BLOCK-3 (ew-keynumber 'E1)) 
							  (list 'SHINTO-PERF-1 (ew-keynumber 'G1)) 
							  (list 'SHINTO-PERF-2 (ew-keynumber 'A1)) 
							  (list 'SHINTO-PERF-3 (ew-keynumber 'B1)) 
							  (list 'SHINTO-PERF-4 (ew-keynumber 'C2)) 
							  (list 'SHINTO-PERF-5 (ew-keynumber 'D2)) 
							  (list 'SHINTO-PERF-6 (ew-keynumber 'E2)) 
							  (list 'SHINTO-PERF-7 (ew-keynumber 'F2)) 
							  (list 'SLAPSTICK-1 (ew-keynumber 'C3)) 
							  (list 'SLAPSTICK-2 (ew-keynumber 'D3)) 
							  (list 'SLAPSTICK-PERF (ew-keynumber 'E3)) 
							  (list 'TEMPO-BLOCK-1 (ew-keynumber 'C4)) 
							  (list 'TEMPO-BLOCK-2 (ew-keynumber 'D4)) 
							  (list 'TEMPO-BLOCK-PERF (ew-keynumber 'E4))))))) 

