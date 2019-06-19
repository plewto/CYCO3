;;;; CYCO plugins sj eastwest sd3 smalldrums tantan.lisp
;;;;

(param tantan nil)

(defun tantan (&key (parent ew-sd3-smalldrums)
		    channel
		    dynamic-map
		    (remarks "SD3 Small Drums Tantan"))
  (setf tantan (make-instrument 'tantan
				:transient t
				:parent parent
				:channel channel
				:dynamic-map dynamic-map
				:remarks remarks
				:keynumber-map (symbolic-keynumber-map
						(list 
						 (list 'open (ew-keynumber 'C1 ))
						 (list 'bass (ew-keynumber 'D1 ))
						 (list 'mute (ew-keynumber 'E1 ))
						 (list 'slap (ew-keynumber 'F1 ))
						 (list 'PALm (ew-keynumber 'G1 ))
						 (list 'back-forth (ew-keynumber 'A1 ))
						 (list 'finger (ew-keynumber 'B1 ))
						 (list 'shell-thumb (ew-keynumber 'C2 ))
						 (list 'shell-finger (ew-keynumber 'D2 ))
						 (list 'perf-short-0 (ew-keynumber 'E2 ))
						 (list 'perf-short-1 (ew-keynumber 'F2 ))
						 (list 'perf-short-2 (ew-keynumber 'G2 ))
						 (list 'perf-short-3 (ew-keynumber 'A2 ))
						 (list 'perf-short-4 (ew-keynumber 'B2 ))
						 (list 'perf-short-5 (ew-keynumber 'C3 ))
						 (list 'perf-long-1 (ew-keynumber 'D3 ))
						 (list 'perf-long-2 (ew-keynumber 'E3 ))
						 (list 'perf-long-3 (ew-keynumber 'F3 ))
						 (list 'perf-long-4 (ew-keynumber 'G3 ))
						 (list 'perf-long-5 (ew-keynumber 'A3 ))
						 (list 'perf-long-6 (ew-keynumber 'B3 ))
						 (list 'perf-long-7 (ew-keynumber 'C4 )))))))
