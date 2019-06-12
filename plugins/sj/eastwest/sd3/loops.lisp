;;;; CYCO plugins sj eastwest sd3 loops.lisp
;;;;

(param sd3-loop nil)

(let ((loop-stats (list
		   (list 'Tibet
			 " 0  6 Minutes in Tibet 100 BPM [00-28]"
			 (ew-keynumber 'C5))
		   (list 'Acapulco
			 " 1  Acapulco 115 BPM [00-38]"
			 (ew-keynumber 'F6))
		   (list 'Acapulco-Again
			 " 2  Acapulco Again 115 BPM [00-19]"
			 (ew-keynumber 'A3))
		   (list 'Beast
			 " 3  Beast Feast 150 BPM [00-23]"
			 (ew-keynumber 'E4))
		   (list 'Cave-Racer
			 " 4  Cave Racer 120 BPM [00-30]"
			 (ew-keynumber 'E5))
		   (list 'Crow-Flies
			 " 5  Crow Flies North 120 BPM [00-29]"
			 (ew-keynumber 'D5))
		   (list 'drunk-spoon
			 " 6  Drunken Spoons 095 BPM [00-09]"
			 (ew-keynumber 'E2))
		   (list 'Fantasy-Bird
			 " 7  Fantasy Bird 120 BPM [00-40]"
			 (ew-keynumber 'A6))
		   (list 'Guru
			 " 8  Guru Chant 100 BPM [00-39]"
			 (ew-keynumber 'G6))
		   (list 'House-On-Fire
			 " 9  House On Fire 090 BPM [00-41]"
			 (ew-keynumber 'B6))
		   (list 'Witch-Doctor
			 "10  Jolly Witch Doctor 115 BPM [00-41]"
			 (ew-keynumber 'B6))
		   (list 'Kongs-Henchmen
			 "11  Kongs Henchmen 105 BPM [00-37]"
			 (ew-keynumber 'E6))
		   (list 'Log-Jamma
			 "12  Log Jamma 115 BPM [00-41]"
			 (ew-keynumber 'B6))
		   (list 'Monasterista
			 "13  Monasterista 100 BPM [00-41]"
			 (ew-keynumber 'B6))
		   (list 'Monte-Carlo
			 "14  Monte Carlo 090 BPM [00-33]"
			 (ew-keynumber 'A5))
		   (list 'Poison-Darts
			 "15  Poison Darts R Us 100 BPM [00-40]"
			 (ew-keynumber 'A6))
		   (list 'Frogs
			 "16  Red Frog Green Frog 120 BPM [00-41]"
			 (ew-keynumber 'B6))
		   (list 'Sambastique
			 "17  Sambastique 100 BPM [00-39]"
			 (ew-keynumber 'G6))
		   (list 'Lion-Grasshopper
			 "18  The Lion and The Grasshopper 100 BPM [00-39]"
			 (ew-keynumber 'G6))
		   (list 'Torture-Brigades
			 "19  Torture Brigades 090 BPM [00-22]"
			 (ew-keynumber 'D4))
		   (list 'Undiscovered-Enemy
			 "20  Undiscovered Enemy 120 BPM [00-48]"
			 (ew-keynumber 'B7))
		   (list 'Roman-Guild
			 "21  Roman Guild 090 BPM [00-37] 24 & 31 missing"
			 (ew-keynumber 'E6))
		   (list 'Undiscovered-Country
			 "22  Undiscovered Country 120 BPM [00-37] 24 missing"
			 (ew-keynumber 'E6)))))
  
  (defun sd3-loop (variation &key (parent ew-sd3-loops)
			     channel dynamic-map remarks)
    (let ((stats (cond ((and (integerp variation)
			     (>= variation 0)
			     (< variation (length loop-stats)))
			(nth variation loop-stats))
		       ((and (symbolp variation)(assoc variation loop-stats))
			(assoc variation loop-stats))
		       (t (cyco-warning
			   (sformat "Invalid sd3-loop selection: ~A" variation)
			   "Using default")
			  (car loop-stats)))))
      (setf sd3-loop (make-instrument 'sd3-loop
				      :transient t
				      :parent parent
				      :channel channel
				      :dynamic-map dynamic-map
				      :keynumber-map (finite-list-keynumber-map
						      (white-keys (ew-keynumber 'c1)
								  (ew-keynumber (third stats))))
				      :remarks (or remarks (second stats))))))

  (defun ?sd3-loop ()
    (format t "SD3-loop options:~%")
    (dolist (s loop-stats)
      (format t "  ~19A index ~A~%" (car s)(second s)))) )
					
		  


