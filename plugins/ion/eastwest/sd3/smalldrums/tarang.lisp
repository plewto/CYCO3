;;;; CYCO plugins sj eastwest sd3 smalldrums tarang.lisp
;;;;

(param tarang nil)

(defun tarang (&key (parent ew-sd3-smalldrums)
		    channel
		    dynamic-map
		    (remarks "SD3 Small Drums Tarang"))
  (setf tarang (make-instrument 'tarang
				:transient t
				:parent parent
				:channel channel
				:dynamic-map dynamic-map
				:remarks remarks
				:keynumber-map (symbolic-keynumber-map
						(list
						 (list 'A-OPEN-1 (ew-keynumber 'C1 ))
						 (list 'A-OPEN-2 (ew-keynumber 'D1 ))
						 (list 'A-RIM  (ew-keynumber 'E1 ))
						 (list 'A-FLAM (ew-keynumber 'F1 ))
						 (list 'A-MUTE (ew-keynumber 'G1 ))
						 (list 'A-ROLL (ew-keynumber 'A1 ))
						 (list 'B-OPEN (ew-keynumber 'C2 ))
						 (list 'B-RIM  (ew-keynumber 'D2 ))
						 (list 'B-MUTE (ew-keynumber 'E2 ))
						 (list 'B-ROLL (ew-keynumber 'F2 ))
						 (list 'C-OPEN (ew-keynumber 'C3 ))
						 (list 'C-RIM  (ew-keynumber 'D3 ))
						 (list 'C-FLAM (ew-keynumber 'E3 ))
						 (list 'C-MUTE (ew-keynumber 'F3 ))
						 (list 'C-ROLL (ew-keynumber 'G3 ))
						 (list 'D-OPEN (ew-keynumber 'C4 ))
						 (list 'D-RIM  (ew-keynumber 'D4 ))
						 (list 'D-FLAM (ew-keynumber 'E4 ))
						 (list 'D-MUTE (ew-keynumber 'F4 ))
						 (list 'D-ROLL (ew-keynumber 'G4 ))
						 (list 'PERF-1 (ew-keynumber 'C5 ))
						 (list 'PERF-2 (ew-keynumber 'D5 ))
						 (list 'PERF-3 (ew-keynumber 'E5 ))
						 (list 'PERF-4 (ew-keynumber 'F5 ))
						 (list 'PERF-5 (ew-keynumber 'G5 ))
						 (list 'PERF-6 (ew-keynumber 'A5 )))))))
