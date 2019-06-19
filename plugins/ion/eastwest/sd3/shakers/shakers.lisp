;;;; CYCO plugins sj eastwest sd3 shakers shakers.lisp
;;;;
;;;; ew-sd3-shakers
;;;;  |
;;;;  +-- ganza
;;;;  +-- bamboo-shaker
;;;;  +-- remo-shaker
;;;;  +-- gourd-rattle
;;;;  |     variation 1
;;;;  |     variation 2
;;;;  |
;;;;  +-- deerhoof
;;;;  +-- esher-rattle
;;;;  +-- rainstick
;;;;  +-- seedpod
;;;;  +-- snake-rattle
;;;;  +-- sioux-rattle
;;;;  +-- torpedo-guiro
;;;;

(load-plugin-file "eastwest/sd3/shakers/ganza")
(load-plugin-file "eastwest/sd3/shakers/bamboo")
(load-plugin-file "eastwest/sd3/shakers/remo")
(load-plugin-file "eastwest/sd3/shakers/gourd")

(param deerhoof nil)
(defun deerhoof (&key (parent ew-sd3-shakers)
		      channel dynamic-map remarks)
  (setf deerhoof (make-instrument 'deerhoof
				  :transient t
				  :parent parent
				  :channel channel
				  :dynamic-map dynamic-map
				  :remarks (or remarks "SD3 Deerhoof Rattel  RR 2")
				  :keynumber-map (circular-list-keynumber-map
						  (white-keys (ew-keynumber 'c1)
							      (ew-keynumber 'b1))))))

(param esher-rattle nil)
(defun esher-rattle (&key (parent ew-sd3-shakers)
			  channel dynamic-map remarks)
  (setf esher-rattle (make-instrument 'esher-rattle
				      :transient t
				      :parent parent
				      :channel channel
				      :dynamic-map dynamic-map
				      :remarks (or remarks "SD3 Esher-Rattle Rattel  RR 1")
				      :keynumber-map (circular-list-keynumber-map
						      (white-keys (ew-keynumber 'c1)
								  (ew-keynumber 'c7))))))
(param rainstick nil)
(defun rainstick (&key (parent ew-sd3-shakers)
		       channel dynamic-map remarks)
  (setf rainstick (make-instrument 'rainstick
				   :transient t
				   :parent parent
				   :channel channel
				   :dynamic-map dynamic-map
				   :remarks (or remarks "SD3 Rainstick Rattel  RR 1")
				   :keynumber-map (circular-list-keynumber-map
						   (white-keys (ew-keynumber 'c1)
							       (ew-keynumber 'f4))))))

(param seedpod nil)
(defun seedpod (&key (parent ew-sd3-shakers)
		      channel dynamic-map remarks)
  (setf seedpod (make-instrument 'seedpod
				  :transient t
				  :parent parent
				  :channel channel
				  :dynamic-map dynamic-map
				  :remarks (or remarks "SD3 Seedpod RR 4")
				  :keynumber-map (circular-list-keynumber-map
						  (white-keys (ew-keynumber 'c1)
							      (ew-keynumber 'g1))))))

(param snake-rattle nil)
(defun snake-rattle (&key (parent ew-sd3-shakers)
		      channel dynamic-map remarks)
  (setf snake-rattle (make-instrument 'snake-rattle
				  :transient t
				  :parent parent
				  :channel channel
				  :dynamic-map dynamic-map
				  :remarks (or remarks "SD3 Snake-Rattle RR 1")
				  :keynumber-map (circular-list-keynumber-map
						  (white-keys (ew-keynumber 'c1)
							      (ew-keynumber 'f6))))))


(param sioux-rattle nil)
(defun sioux-rattle (&key (parent ew-sd3-shakers)
			  channel dynamic-map remarks)
  (setf sioux-rattle (make-instrument 'sioux-rattle
				  :transient t
				  :parent parent
				  :channel channel
				  :dynamic-map dynamic-map
				  :remarks (or remarks "SD3 Sioux-Rattle RR 4")
				  :keynumber-map (symbolic-keynumber-map
						  (list (list 'LONG   (ew-keynumber 'C1 ))
							(list 'SHORT  (ew-keynumber 'D1 ))
							(list 'PERF-1 (ew-keynumber 'E1 ))
							(list 'PERF-2 (ew-keynumber 'F1 ))
							(list 'PERF-3 (ew-keynumber 'G1 ))
							(list 'PERF-4 (ew-keynumber 'A1 ))
							(list 'PERF-5 (ew-keynumber 'B1 )))))))

(param torpedo-guiro nil)
(defun torpedo-guiro (&key (parent ew-sd3-shakers)
			  channel dynamic-map remarks)
  (setf torpedo-guiro (make-instrument 'torpedo-guiro
				  :transient t
				  :parent parent
				  :channel channel
				  :dynamic-map dynamic-map
				  :remarks (or remarks "SD3 Torpedo-Guiro RR 2")
				  :keynumber-map (symbolic-keynumber-map
						  (list (list 'SHAKE-1  (ew-keynumber 'C1 ))
							(list 'SHAKE-2  (ew-keynumber 'D1 ))
							(list 'SHAKE-3  (ew-keynumber 'E1 ))
							(list 'SHAKE-4  (ew-keynumber 'F1 ))
							(list 'SHAKE-5  (ew-keynumber 'G1 ))
							(list 'SHAKE-6  (ew-keynumber 'A1 ))
							(list 'DOUBLE-1 (ew-keynumber 'B1 ))
							(list 'DOUBLE-2 (ew-keynumber 'C2 ))
							(list 'DOUBLE-3 (ew-keynumber 'D2 ))
							(list 'LONG (ew-keynumber 'E2 )))))))








