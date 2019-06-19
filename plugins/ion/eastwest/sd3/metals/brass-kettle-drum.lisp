;;;; CYCO plugins sj eastwest sd3 metals brass-kettle-drum.lisp
;;;;
;;;; brass-kettle-drum
;;;;      (brass-kettle-drum)
;;;;      (brass-kettle-drum-perf)

(param brass-kettle-drum nil)

(defun brass-kettle-drum (&key (parent ew-sd3-metals)
			       channel dynamic-map remarks)
  (setf brass-kettle-drum (make-instrument 'brass-kettle-drum
					   :transient t
					   :parent parent
					   :channel channel
					   :dynamic-map dynamic-map
					   :remarks (or remarks "SD3 Netal Brass Kettle Taya Drum  RR 4")
					   :keynumber-map (symbolic-keynumber-map
							   (list
							    (list 'LOW-OPEN-LH (ew-keynumber 'C1 ))
							    (list 'LOW-OPEN-RH (ew-keynumber 'D1 ))
							    (list 'RIM-RH      (ew-keynumber 'E1 ))
							    (list 'MUTE-LONG   (ew-keynumber 'F1 ))
							    (list 'MUTE-SHORT  (ew-keynumber 'G1 ))
							    (list 'SLAP-RH     (ew-keynumber 'A1 ))
							    (list 'ROLL-MUTE-1 (ew-keynumber 'B1 ))
							    (list 'ROLL-MUTE-2 (ew-keynumber 'C2 ))
							    (list 'ROLL-1 (ew-keynumber 'D2 ))
							    (list 'ROLL-2 (ew-keynumber 'E2 ))
							    (list 'OPEN   (ew-keynumber 'C3 ))
							    (list 'RIM-1  (ew-keynumber 'D3 ))
							    (list 'RIM-2  (ew-keynumber 'E3 ))
							    (list 'OPEN-DOUBLE (ew-keynumber 'F3 ))
							    (list 'SLAP   (ew-keynumber 'G3 ))
							    (list 'MUTE   (ew-keynumber 'A3 ))
							    (list 'ROLL-3 (ew-keynumber 'B3 ))
							    (list 'ROLL-4 (ew-keynumber 'C4 ))
							    (list 'ROLL-5 (ew-keynumber 'D4 )))))))

(defun brass-kettle-drum-perf (&key (parent ew-sd3-metals)
				    channel dynamic-map remarks)
  (setf brass-kettle-drum (make-instrument 'brass-kettle-drum
					   :transient t
					   :parent parent
					   :channel channel
					   :dynamic-map dynamic-map
					   :remarks (or remarks "SD3 Netal Brass Kettle Taya Perf  RR 4")
					   :keynumber-map (circular-keynumber-map
							   (ew-keynumber 'c0)
							   (ew-keynumber 'as3)))))
					   
