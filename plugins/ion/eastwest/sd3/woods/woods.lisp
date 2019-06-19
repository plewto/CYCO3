;;;; CYCO plugin sj eastwest sd3 woods woods.lisp
;;;;
;;;; ew-sd3-woods
;;;;  |
;;;;  +-- balaphone
;;;;  +-- gourd-marimba
;;;;  +-- wood-cowbell
;;;;  +-- japanese-blocks
;;;;  +-- tongue-drum
;;;;  +-- log-drum
;;;;  +-- redwood-tree (full perf)
;;;;  +-- yambu
;;;;  +-- stomping-tubes *** NOT DEFINED ***
;;;;  +-- slit-gong      *** NOT DEFINED ***


(param wooden-cowbell nil)
(defun wooden-cowbell (&key (parent ew-sd3-woods)
			    channel dynamic-map
			    (remarks "SD3 Woods African Wooden Cowbell"))
  (setf wooden-cowbell (make-instrument 'wooden-cowbell
					:transient t
					:parent parent
					:channel channel
					:dynamic-map dynamic-map
					:remarks remarks
					:keynumber-map (circular-list-keynumber-map
							(white-keys (ew-keynumber 'c1)
								    (ew-keynumber 'a1))))))

(param angklungs nil)
(defun angklungs (&key (parent ew-sd3-woods)
		       channel dynamic-map
		       (remarks "SD3 Woods Angklungs"))
  (setf angklungs (make-instrument 'angklungs
				   :transient t
				   :parent parent
				   :channel channel
				   :dynamic-map dynamic-map
				   :remarks remarks
				   :keynumber-map (circular-list-keynumber-map
						   (white-keys (ew-keynumber 'c1)
							       (ew-keynumber 'd5))))))

(param tongue-drum nil)
(defun tongue-drum (&key (parent ew-sd3-woods)
		       channel dynamic-map
		       (remarks "SD3 Woods Tongue Drum"))
  (setf tongue-drum (make-instrument 'tongue-drum
				   :transient t
				   :parent parent
				   :channel channel
				   :dynamic-map dynamic-map
				   :remarks remarks
				   :keynumber-map (symbolic-keynumber-map
						   (list
						    (list 'HIT-1 (ew-keynumber 'C1)) 
						    (list 'HIT-2 (ew-keynumber 'D1)) 
						    (list 'HIT-3 (ew-keynumber 'E1)) 
						    (list 'HIT-4 (ew-keynumber 'F1)) 
						    (list 'HIT-5 (ew-keynumber 'G1)) 
						    (list 'HIT-6 (ew-keynumber 'A1)) 
						    (list 'MUTE-1 (ew-keynumber 'C2)) 
						    (list 'MUTE-2 (ew-keynumber 'D2)) 
						    (list 'MUTE-3 (ew-keynumber 'E2)) 
						    (list 'MUTE-4 (ew-keynumber 'F2)) 
						    (list 'MUTE-5 (ew-keynumber 'G2)) 
						    (list 'MUTE-6 (ew-keynumber 'A2)) 
						    (list 'PERF-01 (ew-keynumber 'C3)) 
						    (list 'PERF-02 (ew-keynumber 'D3)) 
						    (list 'PERF-03 (ew-keynumber 'E3)) 
						    (list 'PERF-04 (ew-keynumber 'F3)) 
						    (list 'PERF-05 (ew-keynumber 'G3)) 
						    (list 'PERF-06 (ew-keynumber 'A3)) 
						    (list 'PERF-07 (ew-keynumber 'B3)) 
						    (list 'PERF-08 (ew-keynumber 'C4)) 
						    (list 'PERF-09 (ew-keynumber 'D4)) 
						    (list 'PERF-10 (ew-keynumber 'E4)) 
						    (list 'PERF-11 (ew-keynumber 'F4)) 
						    (list 'PERF-12 (ew-keynumber 'G4)) 
						    (list 'PERF-13 (ew-keynumber 'A4)) 
						    (list 'PERF-14 (ew-keynumber 'B4))))))) 
						    

(param log-drum nil)
(defun log-drum (&key (parent ew-sd3-woods)
		      channel dynamic-map
		      (remarks "SD3 Woods Log Drum"))
  (setf log-drum (make-instrument 'log-drum
				  :transient t
				  :parent parent
				  :channel channel
				  :dynamic-map dynamic-map
				  :remarks remarks
				  :keynumber-map (circular-list-keynumber-map
						  (white-keys (ew-keynumber 'c1)
							      (ew-keynumber 'f1))))))

(param yambu nil)
(defun yambu (&key (parent ew-sd3-woods)
		      channel dynamic-map
		      (remarks "SD3 Woods Yambu"))
  (setf yambu (make-instrument 'yambu
				  :transient t
				  :parent parent
				  :channel channel
				  :dynamic-map dynamic-map
				  :remarks remarks
				  :keynumber-map (symbolic-keynumber-map
						  (list
						   (list 'BASS (ew-keynumber 'C1)) 
						   (list 'EDGE (ew-keynumber 'D1)) 
						   (list 'FLAM (ew-keynumber 'E1)) 
						   (list 'SLAP-LEFT  (ew-keynumber 'F1)) 
						   (list 'SLAP-RIGHT (ew-keynumber 'G1)) 
						   (list 'ROLL-1 (ew-keynumber 'C2)) 
						   (list 'ROLL-2 (ew-keynumber 'D2)) 
						   (list 'ROLL-3 (ew-keynumber 'E2)) 
						   (list 'ROLL-4 (ew-keynumber 'F2)) 
						   (list 'ROLL-5 (ew-keynumber 'G2)) 
						   (list 'ROLL-6 (ew-keynumber 'A2)) 
						   (list 'ROLL-7 (ew-keynumber 'B2)) 
						   (list 'ROLL-8 (ew-keynumber 'C3))))))) 
(load-plugin-file "eastwest/sd3/woods/balaphone")
(load-plugin-file "eastwest/sd3/woods/jblocks")
(load-plugin-file "eastwest/sd3/woods/redwood")
