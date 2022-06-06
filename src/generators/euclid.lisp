;;;; CYCO generators/euclid.lisp
;;;;
;;;; Generates Euclid pattern.
;;;; https://en.wikipedia.org/wiki/Euclidean_rhythm
;;;; https://dbkaplun.github.io/euclidean-rhythm/
;;;;
;;;; TODO: Update documentation.

(in-package :cyco)



(defun euclid->binary-cuelist (points &key (shift 0)(timesig nil)(use-subbeats t))
  (let* ((tsig (select-time-signature timesig))
	 (length (cue-points tsig (not use-subbeats)))
	 (increment (/ length points))
	 (value shift)
	 (acc '())
	 (cue ""))
    (while (< value length)
      (push (truncate (round value)) acc)
      (setf value (+ value increment)))
    (loop for i from 0 below length do
	  (setf cue (str+ cue (if (member i acc) "1" "0"))))
    (let* ((symlist (list (cons 'euclid cue)))
	   (bc (bincue :symbols symlist :timesig tsig :use-subbeats use-subbeats)))
      (bincue-translate bc (list 'euclid)))))




(setf (documentation 'euclid->binary-cuelist 'function)
      "Generates a Euclidean-rhythm cuelist for use with BINCUE.

points - Number of events, should be less then or equal to the -total-
         number of time-signature subbeats.  The time-signature method
         (CUE-POINTS tsig) returns the maximum allowed value.

:shift   - Number of subbeats to shift rhythm, default 0.
:timesig - Reference time-signature, may be one of the following.
           1) An instance of TIME-SIGNATURE.
           2) A list of form (BARS BEATS SUBBEATS)
           3) Defaults to either the current project section.
              If there is no current section defaults to *PROJECT*.
:use-subbeats - If true use time-signature subbeats for base unit,
           Otherwise use tsubbeats. Default T.")
