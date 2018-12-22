;;;; cyco-oud monochord
;;;;
;;;; Defines key-range in terms of a monochord with optional capo

(defstruct monochord
  root-key				;; integer
  fret-count				;; integer 
  capo-position) 			;; integer

;; Maximum fret-position relative to the capo.
;;
(defmethod monochord-max-position ((mc monochord))
  (- (monochord-fret-count mc)
     (monochord-capo-position mc)))

(defmethod monochord-min-keynumber ((mc monochord))
  (+ (monochord-root-key mc)
     (monochord-capo-position mc)))

(defmethod monochord-max-keynumber ((mc monochord))
  (+ (monochord-root-key mc)
     (monochord-fret-count mc)))

;; Determine key-number from fret-position relative to capo.
;; Special positions:  'O  --> open string
;;                     'X  --> muted string --> +REST+
;;                     'R  --> same as 'X
;;
(defmethod monochord-keynumber ((mc monochord)(position symbol))
  (cond ((symbol-eq-p position 'o)
	 (+ (monochord-root-key mc)
	    (monochord-capo-position mc)))
	((symbol-eq-p position 'x) +REST+)
	((symbol-eq-p position 'r) +REST+)
	(t (cyco-error "INVALID MONOCHORD POSITION: ~A" position)
	   +REST+)))

(defmethod monochord-keynumber ((mc monochord)(position integer))
  (cond ((minusp position) +REST+)
	((> position (monochord-max-position mc)) +REST+)
	(t (+ (monochord-root-key mc)
	      (monochord-capo-position mc)
	      position))))

;; Returns list of possible fret-positions for given pitch-class.
;;
(defmethod monochord-key-positions ((mc monochord)(pclass integer))
  (let ((acc '())
	(target (pitch-class pclass)))
    (if (not (minusp target))
	(let ((pos 0)
	      (pos-delta 1))
	  (while (< pos (monochord-max-position mc))
	    (let ((pc (pitch-class (monochord-keynumber mc pos))))
	      (if (= pc target)
		  (progn
		    (push pos acc)
		    (setf pos-delta 12))))
	    (setf pos (+ pos pos-delta)))))
    (reverse acc)))
	    
(defmethod monochord-key-positions ((mc  monochord)(pclass symbol))
  (cond ((symbol-eq-p pclass 'x) nil)
	((symbol-eq-p pclass 'r) nil)
	((symbol-eq-p pclass 'o)(monochord-key-positions mc 0))
	(t (monochord-key-positions mc (pitch-class pclass)))))


(defmethod monochord->string ((mc monochord) &optional pos)
  (let* ((white-keys #("C " "Db" "D " "Eb"
	 	       "E " "F " "Gb" "G "
	 	       "Ab" "A " "Bb" "B "))
	 (acc (sformat "~3A ||" (keyname (monochord-root-key mc)))))
    (dotimes (p (1+ (monochord-fret-count mc)))
      (cond ((< p (monochord-capo-position mc))
	     (setf acc (str+ acc "  |")))
	    ((and (integerp pos) (= p (+ pos (monochord-capo-position mc))))
	     (setf acc (str+ acc "XX|")))
	    (t (let ((pc (pitch-class (+ (monochord-root-key mc) p))))
		 (setf acc (str+ acc (sformat "~2A|" (aref white-keys pc))))))))
    (if pos
	(sformat "~A --> ~3A" acc (keyname (monochord-keynumber mc pos)))
      acc)))
