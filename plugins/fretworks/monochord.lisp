;;;; CYCO fretworks monochord.
;;;;
;;;; Defines key number range based on root-key and fret-count.
;;;; key-numbers less then root-key or greater then root-key+fret-count are
;;;; treated as rest.
;;;;
;;;; Two special symbolic key-numbers are defined:
;;;;
;;;;   X - rest
;;;;   O - open string = root-key
;;;;

(defstruct monochord
  root-key
  fret-count)

(defmethod ->string ((mc monochord))
  (sformat "MONO(root ~A frets ~D)" 
	   (monochord-root-key mc)
	   (monochord-fret-count mc)))

(defmethod monochord-keynumber ((mc monochord)(position t)(capo integer))
  (let ((rs (cond ((symbol-eq-p position 'O)(monochord-root-key mc))
		  ((symbol-eq-p position 'X) +REST+)
		  ((not (integerp position))
		   (cyco-error (sformat "Invalid monochord position: ~A" position)))
		  ((minusp position) +REST+)
		  ((> position (monochord-fret-count mc))
		   +REST+)
		  (t (+ (monochord-root-key mc) (+ capo position))))))
    rs))
	
(defmethod key-positions ((mc monochord)(key t) &key (no-duplicates))
  (dismiss no-duplicates)
  (let* ((pc (pitch-class key))
	 (root (monochord-root-key mc))
	 (diff (- pc root))
	 (position (if (minusp diff)(+ 12 diff) diff))
	 (acc '()))
    (while (< position (monochord-fret-count mc))
      (push position acc)
      (setf position (+ 12 position)))
    (reverse acc)))
