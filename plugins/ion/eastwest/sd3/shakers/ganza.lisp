;;;; CYCO plugins sj eastwest sd3 shakers ganza
;;;;


(param ganza nil)
(defun ganza (&key (parent ew-sd3-shakers)
		   channel dynamic-map remarks)
  (setf ganza (make-instrument 'ganza
			       :transient t
			       :parent parent
			       :channel channel
			       :dynamic-map dynamic-map
			       :remarks (or remarks "SD3 Ganza RR 2")
			       :keynumber-map (symbolic-keynumber-map
					       (list
						 (list 'FRONT-BACK-1 (ew-keynumber 'C1 ))
						 (list 'FRONT-BACK-2 (ew-keynumber 'D1 ))
						 (list 'FRONT-BACK-3 (ew-keynumber 'E1 ))
						 (list 'FRONT-1 (ew-keynumber 'F1 ))
						 (list 'FRONT-2 (ew-keynumber 'G1 ))
						 (list 'BACK-1  (ew-keynumber 'A1 ))
						 (list 'BACK-2  (ew-keynumber 'B1 ))
						 (list 'SHORT   (ew-keynumber 'C2 ))
						 (list 'LONG    (ew-keynumber 'D2 ))
						 (list 'SHAKE-01  (ew-keynumber 'E2 ))
						 (list 'SHAKE-02  (ew-keynumber 'F2 ))
						 (list 'SHAKE-03  (ew-keynumber 'G2 ))
						 (list 'SHAKE-04  (ew-keynumber 'A2 ))
						 (list 'SHAKE-05  (ew-keynumber 'B2 ))
						 (list 'SHAKE-06  (ew-keynumber 'C3 ))
						 (list 'SHAKE-07  (ew-keynumber 'D3 ))
						 (list 'SHAKE-08  (ew-keynumber 'E3 ))
						 (list 'SHAKE-09  (ew-keynumber 'F3 ))
						 (list 'SHAKE-10  (ew-keynumber 'G3 ))
						 (list 'SHAKE-11  (ew-keynumber 'A3 ))
						 (list 'SHAKE-12  (ew-keynumber 'B3 ))
						 (list 'SHAKE-13  (ew-keynumber 'C4 ))
						 (list 'SHAKE-14  (ew-keynumber 'D4 )))))))
