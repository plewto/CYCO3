;;;; CYCO plugins sj eastwest sd3 metals metals.lisp
;;;;
;;;; ew-sd3-metals
;;;;  |
;;;;  +-- african-boat-bells
;;;;  +-- agogo
;;;;  +-- asian-crotales (pentatonic)
;;;;  +-- earth-plates
;;;;  +-- meditation-bowls
;;;;  +-- ghanaian-bells
;;;;  +-- water-gong
;;;;  +-- ice-bells
;;;;  +-- japanese-bells
;;;;  +-- slit-gong
;;;;  +-- tinshaw
;;;;  +-- spring-drum
;;;;  +-- turkish-copper-pot
;;;;  +-- water-bowls
;;;;  +-- zil-bells
;;;;  +-- berimabau (AF B)
;;;;  +-- bass-bowls (tibet turkey)
;;;;  +-- brass-kettle-drum
;;;;  |     (brass-kettle-drum)
;;;;  |     (brass-kettle-drum-perf)
;;;;  |
;;;;  +-- cengceng (klang perf)
;;;;  +-- chimes (brass burma cymbal wind)
;;;;  +-- dark-star-bells (hits tuned perf)
;;;;  +-- doorstop (1 2 3)
;;;;  +-- englehart-bells (bells tuned stick-tuned)
;;;;  +-- dragon-chaser
;;;;  +-- finger-cymbals
;;;;  +-- mongolian-chimes (1 2 perf-1 perf-2 chromatic)
;;;;  +-- ratchet (ratchet indonesian)
;;;;  +-- supra (bowls fx)



(param african-boat-bells nil)
(defun african-boat-bells (&key (parent ew-sd3-metals)
				channel dynamic-map remarks)
  (setf african-boat-bells (make-instrument 'african-boat-bells
					    :transient t
					    :parent parent
					    :channel channel
					    :dynamic-map dynamic-map
					    :keynumber-map (circular-list-keynumber-map
							    (white-keys (ew-keynumber 'c1)
									(ew-keynumber 'c2)))
					    :remarks (or remarks "SD3 Metals African Boat Bells   RR 1"))))

(param agogo nil)
(defun agogo (&key (parent ew-sd3-metals)
		   channel dynamic-map remarks)
  (setf agogo (make-instrument 'agogo
			       :transient t
			       :parent parent
			       :channel channel
			       :dynamic-map dynamic-map
			       :keynumber-map (circular-list-keynumber-map
					       (white-keys (ew-keynumber 'c1)
							   (ew-keynumber 'c2)))
			       :remarks (or remarks "SD3 Metals Agogo   RR 4"))))

(param asian-crotales nil)
(defun asian-crotales (&key (parent ew-sd3-metals)
		   channel dynamic-map remarks)
  (setf asian-crotales (make-instrument 'asian-crotales
			       :transient t
			       :parent parent
			       :channel channel
			       :dynamic-map dynamic-map
			       :keynumber-map (symbolic-keynumber-map
					       (list (list 'D  (ew-keynumber 'C1))
						     (list 'F  (ew-keynumber 'D1))
						     (list 'DS (ew-keynumber 'E1))
						     (list 'D2 (ew-keynumber 'F1))
						     (list 'A  (ew-keynumber 'G1))))
			       :remarks (or remarks "SD3 Metals Asian-Crotales   RR 4 (petatonic scale)"))))

(param earth-plates nil)
(defun earth-plates (&key (parent ew-sd3-metals)
			  channel dynamic-map remarks)
  (setf earth-plates (make-instrument 'earth-plates
				      :transient t
				      :parent parent
				      :channel channel
				      :dynamic-map dynamic-map
				      :keynumber-map (circular-list-keynumber-map
						      (white-keys (ew-keynumber 'c1)
								  (ew-keynumber 'f3)))
				      :remarks (or remarks "SD3 Metals Earth Plates   RR 4"))))

(param meditation-bowls nil)
(defun meditation-bowls (&key (parent ew-sd3-metals)
		   channel dynamic-map remarks)
  (setf meditation-bowls (make-instrument 'meditation-bowls
			       :transient t
			       :parent parent
			       :channel channel
			       :dynamic-map dynamic-map
			       :keynumber-map (circular-list-keynumber-map
					       (white-keys (ew-keynumber 'c1)
							   (ew-keynumber 'f3)))
			       :remarks (or remarks "SD3 Metals Deep Meditation Bowls   RR 1"))))


(param ghanaian-bells nil)
(defun ghanaian-bells (&key (parent ew-sd3-metals)
		   channel dynamic-map remarks)
  (setf ghanaian-bells (make-instrument 'ghanaian-bells
			       :transient t
			       :parent parent
			       :channel channel
			       :dynamic-map dynamic-map
			       :keynumber-map (circular-list-keynumber-map
					       (white-keys (ew-keynumber 'c1)
							   (ew-keynumber 'd3)))
			       :remarks (or remarks "SD3 Metals Ghanaian Bells   RR 4"))))

(param water-gong nil)
(defun water-gong (&key (parent ew-sd3-metals)
			channel dynamic-map remarks)
  (setf water-gong (make-instrument 'water-gong
				    :transient t
				    :parent parent
				    :channel channel
				    :dynamic-map dynamic-map
				    :keynumber-map (circular-list-keynumber-map
						    (white-keys (ew-keynumber 'c1)
								(ew-keynumber 'd6)))
				    :remarks (or remarks "SD3 Metals Gongs in Water  RR 1"))))

(param ice-bells nil)
(defun ice-bells (&key (parent ew-sd3-metals)
			channel dynamic-map remarks)
  (setf ice-bells (make-instrument 'ice-bells
				   :transient t
				   :parent parent
				   :channel channel
				   :dynamic-map dynamic-map
				   :keynumber-map (circular-list-keynumber-map
						   (white-keys (ew-keynumber 'c1)
							       (ew-keynumber 'b5)))
				   :remarks (or remarks "SD3 Metals Ice Bells  RR 1"))))

(param japanese-bells nil)
(defun japanese-bells (&key (parent ew-sd3-metals)
			channel dynamic-map remarks)
  (setf japanese-bells (make-instrument 'japanese-bells
					:transient t
					:parent parent
					:channel channel
					:dynamic-map dynamic-map
					:keynumber-map (circular-list-keynumber-map
							(white-keys (ew-keynumber 'c1)
								    (ew-keynumber 'b1)))
					:remarks (or remarks "SD3 Metals Japanese Bells  RR 1"))))

(param slit-gong nil)
(defun slit-gong (&key (parent ew-sd3-metals)
			channel dynamic-map remarks)
  (setf slit-gong (make-instrument 'slit-gong
				   :transient t
				   :parent parent
				   :channel channel
				   :dynamic-map dynamic-map
				   :keynumber-map (symbolic-keynumber-map
						   (list (list 'HIT-1  (ew-keynumber 'C1 ))
							 (list 'MUTE-1 (ew-keynumber 'D1 ))
							 (list 'HIT-2  (ew-keynumber 'E1 ))
							 (list 'MUTE-2 (ew-keynumber 'F1 ))
							 (list 'HIT-3  (ew-keynumber 'G1 ))
							 (list 'MUTE-3 (ew-keynumber 'A1 ))
							 (list 'HIT-4  (ew-keynumber 'B1 ))
							 (list 'MUTE-4 (ew-keynumber 'C2 ))
							 (list 'PERF-1 (ew-keynumber 'D2 ))
							 (list 'PERF-2 (ew-keynumber 'E2 ))
							 (list 'PERF-3 (ew-keynumber 'F2 ))
							 (list 'PERF-4 (ew-keynumber 'G2 ))
							 (list 'PERF-5 (ew-keynumber 'A2 ))
							 (list 'PERF-6 (ew-keynumber 'B2 ))
							 (list 'PERF-7 (ew-keynumber 'C3 ))))
				   :remarks (or remarks "SD3 Metals Metal Slit Gong  RR 2"))))

(param tinshaw nil)
(defun tinshaw (&key (parent ew-sd3-metals)
		     channel dynamic-map remarks)
  (setf tinshaw (make-instrument 'tinshaw
				 :transient t
				 :parent parent
				 :channel channel
				 :dynamic-map dynamic-map
				 :keynumber-map (circular-list-keynumber-map
						 (white-keys (ew-keynumber 'c1)
							     (ew-keynumber 'f2)))
				 :remarks (or remarks "SD3 Metals Nepalese Tinshaw"))))

(param spring-drum nil)
(defun spring-drum (&key (parent ew-sd3-metals)
			channel dynamic-map remarks)
  (setf spring-drum (make-instrument 'spring-drum
				     :transient t
				     :parent parent
				     :channel channel
				     :dynamic-map dynamic-map
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
						      (list 'HIT-12 (ew-keynumber 'G2 ))
						      (list 'HIT-13  (ew-keynumber 'A2 ))
						      (list 'SHELL (ew-keynumber 'B1 ))
						      (list 'MUTE-SCRAP (ew-keynumber 'C2 ))
						      (list 'SCRAPE-1 (ew-keynumber 'D2 ))
						      (list 'SCRAPE-2 (ew-keynumber 'E2 ))
						      (list 'SCRAPE-3 (ew-keynumber 'F2 ))
						      (list 'PERF-01 (ew-keynumber 'A2 ))
						      (list 'PERF-02 (ew-keynumber 'B2 ))
						      (list 'PERF-03 (ew-keynumber 'C3 ))
						      (list 'PERF-04 (ew-keynumber 'D3 ))
						      (list 'PERF-05 (ew-keynumber 'E3 ))
						      (list 'PERF-06 (ew-keynumber 'F3 ))
						      (list 'PERF-07 (ew-keynumber 'G3 ))
						      (list 'PERF-08 (ew-keynumber 'A3 ))
						      (list 'PERF-09 (ew-keynumber 'B3 ))
						      (list 'PERF-10 (ew-keynumber 'C4 ))
						      (list 'PERF-11 (ew-keynumber 'D4 ))
						      (list 'PERF-12 (ew-keynumber 'E4 ))
						      (list 'PERF-13 (ew-keynumber 'F4 ))))
				     :remarks (or remarks "SD3 Spring-Drum"))))


(param turkish-copper-pot nil)
(defun turkish-copper-pot (&key (parent ew-sd3-metals)
				channel dynamic-map remarks)
  (setf turkish-copper-pot (make-instrument 'turkish-copper-pot
					    :transient t
					    :parent parent
					    :channel channel
					    :dynamic-map dynamic-map
					    :keynumber-map (circular-list-keynumber-map
							    (white-keys (ew-keynumber 'c1)
									(ew-keynumber 'f1)))
					    :remarks (or remarks "SD3 Metals Turkish Copper Pot"))))

(param water-bowls nil)
(defun water-bowls (&key (parent ew-sd3-metals)
				channel dynamic-map remarks)
  (setf water-bowls (make-instrument 'water-bowls
				     :transient t
				     :parent parent
				     :channel channel
				     :dynamic-map dynamic-map
				     :keynumber-map (circular-list-keynumber-map
						     (white-keys (ew-keynumber 'c1)
								 (ew-keynumber 'f2)))
				     :remarks (or remarks "SD3 Metals Water Bowls"))))

(param zil-bells nil)
(defun zil-bells (&key (parent ew-sd3-metals)
				channel dynamic-map remarks)
  (setf zil-bells (make-instrument 'zil-bells
				   :transient t
				   :parent parent
				   :channel channel
				   :dynamic-map dynamic-map
				   :keynumber-map (circular-list-keynumber-map
						   (white-keys (ew-keynumber 'c1)
							       (ew-keynumber 'e2)))
				   :remarks (or remarks "SD3 Metals Zil Bells"))))

(param dragon-chaser nil)
(defun dragon-chaser (&key (parent ew-sd3-metals)
			   channel dynamic-map remarks)
  (setf dragon-chaser (make-instrument 'dragon-chaser
				       :transient t
				       :parent parent
				       :channel channel
				       :dynamic-map dynamic-map
				       :keynumber-map (circular-list-keynumber-map
						       (white-keys (ew-keynumber 'c1)
								   (ew-keynumber 'e1)))
				       :remarks (or remarks "SD3 Metals Dragon Chaser"))))

(load-plugin-file "eastwest/sd3/metals/berimbau")
(load-plugin-file "eastwest/sd3/metals/brass-bowls")
(load-plugin-file "eastwest/sd3/metals/brass-kettle-drum")
(load-plugin-file "eastwest/sd3/metals/cengceng")
(load-plugin-file "eastwest/sd3/metals/chimes")
(load-plugin-file "eastwest/sd3/metals/dark-star")
(load-plugin-file "eastwest/sd3/metals/doorstop")
(load-plugin-file "eastwest/sd3/metals/englehart")
(load-plugin-file "eastwest/sd3/metals/finger-cymbals")
(load-plugin-file "eastwest/sd3/metals/mongolian-chimes")
(load-plugin-file "eastwest/sd3/metals/ratchet")
(load-plugin-file "eastwest/sd3/metals/supra")
