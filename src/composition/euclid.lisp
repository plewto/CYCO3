;;;; CYCO generators/euclid.lisp
;;;;
;;;; Generates Euclid pattern.
;;;; https://en.wikipedia.org/wiki/Euclidean_rhythm
;;;; https://dbkaplun.github.io/euclidean-rhythm/
;;;;
;;;; NOTE: Euclid was originally based on the GENERATOR class and was always
;;;;       clunky.  It has been moved to a plugin called LEGACY-EUCLID.
;;;;       


(in-package :cyco)

(defun euclid->binary-cuelist (length points &key (shift 0))
  (let ((increment (/ length points))
	(value shift)
	(acc '())
	(cuelist ""))
    (while (< value length)
      (push (round value) acc)
      (setf value (+ value increment)))
    (loop for i from 0 below length do
	  (setf cuelist (str+ cuelist (if (member i acc) "1" "0"))))
    cuelist))


(defun euclid (points &key (shift 0)(timesig nil)(use-subbeats t))
  (let* ((tsig (select-time-signature timesig))
	 (gamut (->vector (cue-gamut tsig (not use-subbeats))))
	 (length (length gamut))
	 (bcuelist (euclid->binary-cuelist length points :shift shift))
	 (acc '()))
    (dotimes (i (length bcuelist))
      (if (char= (char bcuelist i) #\1)
	  (push (aref gamut i) acc)))
    (reverse acc)))


(labels ((head (cuelist split)
	       (remove-if #'(lambda (cue)(> (car cue) split)) cuelist))

	 (tail (cuelist split)
	       (remove-if #'(lambda (cue)(<= (car cue) split)) cuelist)) )
	
	(defun euclid2 (p1 p2 &key (shift1 0)(shift2 0)(timesig nil)(split nil)(use-subbeats t))
	  (let* ((tsig (select-time-signature timesig))
		 (e1 (euclid p1 :shift shift1 :timesig tsig :use-subbeats use-subbeats))
		 (e2 (euclid p2 :shift shift2 :timesig tsig :use-subbeats use-subbeats))
		 (bars (bars tsig))
		 (crossover (limit (or split (round (/ bars 2))) 0 (1- bars))))
	    (append (head e1 crossover)
		    (tail e2 crossover)))))


(setf (documentation 'euclid->binary-cuelist 'function)
      "Returns string of binary values for Euclidean rhythm.
length - number of base units, 0 < length
points - number of events, 1 < points <= length
:shift - amount of rhythmic shift in base units, default 0.
Returns String")

(setf (documentation 'euclid 'function)
      "Generates cue-list from Euclidean rhythm.
points   - Number of events. 0 < points.
:shift   - Amount of rhythmic shift in base units, default 0.
:timesig - Reference time-signature may be one of the following.
           1) An instance of TIME-SIGNATURE
           2) A list of form (bars beats subbeats)
           3) nil, 
               A) Defaults to either the current-section of *project*
               B) If there is no current-section, uses *project*
               C) If *project* is nil uses a 2-bar 4/4 fallback.

:use-subbeats - Boolean if true uses time-signature subbeats as the 
          as the rhythmic unit, otherwise use subbeats. Default t.

Returns list.")


(setf (documentation 'euclid2 'function)
      "Splices 2 Euclidean rhythms together.
p1 - Point count for rhythm 1, 0 < p1.
p2 - Point count for rhythm 2, 0 < p2.
:shift1  - Rhythmic shift for rhythm 1, default 0.
:shift2  - Rhythmic shift for rhythm 2, default 0.
:timesig - Reference time-signature.  See EUCLID function. 
:split   - Split point in bars between the two rhythms.
           Defaults to halfway point.
:use-subbeats - As per EUCLID function.

Returns list.")
