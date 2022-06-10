;;;; CYCO composition/euclid.lisp
;;;;
;;;; Generates Euclid pattern.
;;;; https://en.wikipedia.org/wiki/Euclidean_rhythm
;;;; https://dbkaplun.github.io/euclidean-rhythm/
;;;;
;;;; NOTE: Euclid was originally based on the GENERATOR class and was always
;;;;       clunky.  It has been moved to a plugin called LEGACY-EUCLID.
;;;;       


(in-package :cyco)


(labels ((points-arg (points length)
		     (cond ((integerp points)
			    points)
			   ((floatp points)
			    (limit (round (* points length)) 0 length))
			   (t (random length))))

	 (shift-arg (shift length)
	 	       (cond ((integerp shift)
	 		      shift)
	 		     ((floatp shift)
	 		      (limit (round (* shift length)) 0 length))
	 		     (t (let* ((r (random 1.0))
	 			       (r3 (* r r r)))
	 			  (shift-arg r3 length)))))

	 (invert-cuelist (flag bincuelist)
			(if flag
			    (let ((s ""))
			      (dotimes (i (length bincuelist))
				(setf s (str+ s (if (char= (char bincuelist i) #\0) #\1 #\0))))
			      s)
			  bincuelist)) )

	(defun euclid->binary-cuelist (length points &key (shift 0)(invert nil))
	  (setf points (points-arg points length))
	  (setf shift (shift-arg shift length))
	  (let ((increment (/ length points))
		(value 0)
		(acc '())
		(bincuelist ""))
	    (while (< value length)
	      (push (round value) acc)
	      (setf value (+ value increment)))
	    (loop for i from 0 below length do
		  (setf bincuelist (str+ bincuelist (if (member i acc) "1" "0"))))
	    (rotate (invert-cuelist invert bincuelist) shift)))


	(defun euclid (points &key (shift 0)(invert nil)(timesig nil)(use-subbeats t))
	  (let* ((tsig (select-time-signature timesig))
		 (gamut (->vector (cue-gamut tsig (not use-subbeats))))
		 (length (length gamut))
		 (bcuelist (euclid->binary-cuelist length points :shift shift :invert invert))
		 (acc '()))
	    (dotimes (i (length bcuelist))
	      (if (char= (char bcuelist i) #\1)
		  (push (aref gamut i) acc)))
	    (reverse acc)))


	(defun euclid2 (p1 p2 &key (s1 0)(s2 0)(inv1 nil)(inv2 nil)(timesig nil)(split nil)(use-subbeats t))
	  (let* ((tsig (select-time-signature timesig))
		 (e1 (euclid p1 :shift s1 :invert inv1 :timesig tsig :use-subbeats use-subbeats))
		 (e2 (euclid p2 :shift s2 :invert inv2 :timesig tsig :use-subbeats use-subbeats))
		 (bars (bars tsig))
		 (crossover (limit (or split (round (/ bars 2))) 0 (1- bars))))
	    (append (remove-if #'(lambda (cue)(> (car cue) crossover)) e1)
		    (remove-if #'(lambda (cue)(<= (car cue) crossover)) e2)))) )


(setf (documentation 'euclid->binary-cuelist 'function)
      "Generates 'binary' cuelist using Euclid's formula.
length  - Number of base time-units.  0 < length.
points  - Number of events.  points may be one of following:
          A) integer, absolute number of events. 0 < points <= length.
          B) float, event count relative to length. 0.0 < points <= 1.0
          C) nil, use random value.
:shift  - Amount of cuelist rotation, may be one of following:
          A) integer, absolute number of steps.
          B) float, steps relative to length.  0.0 <= shift <= 1.0
          C) nil, use random value.
          Default 0.
:invert - Boolean, if true invert binary values.

Returns string of binary digits '0's or '1's.")


(setf (documentation 'euclid 'function)
      "Generates cuelist using Euclid's formula
points   - Number of events, may integer, float or nil as per EUCLID->BINARY-CUELIST.
:shift   - List rotation amount, as per EUCLID->BINARY-CUELIST, default 0.
:invert  - Boolean, if true invert event selection. Default nil.
:timesig - Reference time signature, may be one of.
           A) An instance of TIME-SIGNATURE
           B) List of form (bars beats subbeats)
           C) NIL
              1) Use current-section of *project*
              2) If there is no current-section, use *project*
              3) If *project* is nil use fallback (2 4 4)
:use-subbeats - Boolean, if true use subbeats as basic time unit, otherwise use
                tsubbeats. Default t.

Returns cuelist in 'BAR' format  ((bar beat subbeat) ...)")


(setf (documentation 'euclid2 'function)
      "Generates sequence of two Euclid rhythms.
Euclid2 splices together 2 Euclid sequences. Most arguments have identical 
usage as the EUCLID function.

p1     - point count 1, as per EUCLID.
p2     - point count 2.
:s1    - shift amount 1, as per EUCLID.
:s2    - shift amount 2
:inv1  - invert 1, as per EUCLID
:inv2  - invert 2
:split - Splice point bar number.  Euclid sequence 1 is used for all bars <= split.
         Euclid sequence 2 for all bars after split.
         Defaults to midway point.
        
:timesig - Reference time-signature, as per EUCLID.
:use-subbeats - As per EUCLID.

Returns cuelist in 'BAR' format ((bar beta subbeat) ...)")
