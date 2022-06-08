;;;; cyco composition binball.lisp
;;;;
;;;; Defines combined structures for BINCUE+QBALL and BINCUE+XBALL.
;;;;
;;;; Changes: Now returns two values, the part object and the cue-list.

(in-package :cyco)


;; (euclid points shift)
;; (euclid2 p1 p2 split s1 s2)
;;
;; TODO: Documentation
(defun binball-cuelist (cuelist bincue time-signature use-subbeats)
  (cond ((eq (car cuelist) 'euclid)
	 (let ((points (or (second cuelist) 1))
	       (shift (or (third cuelist) 0)))
	   (euclid points :shift shift :timesig time-signature :use-subbeats use-subbeats)))

	((eq (car cuelist) 'euclid2)
	 (let ((p1 (or (second cuelist) 1))
	       (p2 (or (third cuelist) 1))
	       (split (fourth cuelist))
	       (s1 (or (fifth cuelist) 0))
	       (s2 (or (sixth cuelist) 0)))
	   (euclid2 p1 p2 :shift1 s1 :shift2 s2 :split split
		    :timesig time-signature :use-subbeats use-subbeats)))

	(t (bincue-translate bincue cuelist))))
	

(defmacro binball (name instruments &key
			section shuffle shift tempo unit bars beats
			subbeats render-once transposable reversible 
			cue key dur amp reset-on-repeat remarks symbols
			(use-subbeats t) print-cue-list)
  `(let ((parent (or ,section (property *project* :current-section))))
     (part-banner (name parent) ',name)
     (let* ((tsig (if (or ,bars ,beats ,subbeats)
		      (time-signature :bars (or ,bars (bars parent))
				      :beats (or ,beats (beats parent))
				      :subbeats (or ,subbeats (subbeats parent)))
		    parent))
	    (bincue (bincue :symbols ,symbols :timesig tsig :use-subbeats ,use-subbeats))
	    (bar-cuelist (binball-cuelist ,cue bincue tsig ,use-subbeats))
	    (qb (make-qball ',name ,instruments
			    :section parent
			    :cuefn #'bar
			    :shuffle (or ,shuffle #'no-shuffle)
			    :shift (or ,shift 0)
			    :render-once ,render-once
			    :transposable ,transposable
			    :reversible ,reversible
			    :tempo ,tempo
			    :unit ,unit
			    :bars ,bars
			    :beats ,beats
			    :subbeats ,subbeats
			    :cue bar-cuelist
			    :key (or ,key '(60))
			    :dur (or ,dur '(q))
			    :amp (or ,amp '(fff))
			    :reset-on-repeat ,reset-on-repeat
			    :remarks (->string (or ,remarks "")))))
       (if ,print-cue-list (pprint-cue-list bar-cuelist (name qb)))
       (defparameter ,name qb)
       (values qb bar-cuelist))))

		      
(defmacro binxball (name instrument &key
			 section shuffle shift render-once tempo unit bars beats
			 subbeats transposable reversible chord-model cue key
			 dur amp chord inversion octave strum end-together
			 direction reset-on-repeat  symbols remarks
			 print-cue-list (use-subbeats t))
  `(let ((parent (or ,section (property *project* :current-section))))
     (part-banner (name parent) ',name)
     (let* ((tsig (if (or ,bars ,beats ,subbeats)
		      (time-signature :bars (or ,bars (bars parent))
				      :beats (or ,beats (beats parent))
				      :subbeats (or ,subbeats (subbeats parent)))
		    parent))
	    (bincue (bincue :symbols ,symbols :timesig tsig :use-subbeats ,use-subbeats))
	    (bar-cuelist (binball-cuelist ,cue bincue tsig ,use-subbeats))
	    (xb (make-xball ',name ,instrument
			    :section parent
			    :cuefn #'bar
			    :tempo ,tempo
			    :unit ,unit
			    :shuffle (or ,shuffle #'no-shuffle)
			    :shift (or ,shift 0)
			    :render-once ,render-once
			    :transposable ,transposable
			    :reversible ,reversible
			    :chord-model (or ,chord-model *chord-table*)
			    :cue bar-cuelist
			    :key (or ,key '(60))
			    :dur (or ,dur '(q))
			    :amp (or ,amp '(fff))
			    :chord (or ,chord '([solo]))
			    :inversion (or ,inversion '(0))
			    :octave (or ,octave '(0))
			    :strum (or ,strum '(0.01))
			    :end-together (or ,end-together nil)
			    :direction (or ,direction '(down))
			    :reset-on-repeat ,reset-on-repeat
			    :remarks (->string (or ,remarks "")))))
       (if ,print-cue-list (pprint-cue-list bar-cuelist (name xb)))
       (defparameter ,name xb)
       (values xb bar-cuelist))))
				 
       
(setf (documentation 'binball 'function)
      "TODO: Documentation does not include euclid2 return values.

Combines QBALL with integral BINCUE binary cue-list translator.
      All arguments are identical to those for QBALL and BINCUE with the
      following exceptions:

      1) :cuefn argument is not available, the QBALL uses the BAR function.
      
      2) The :cue argument takes a 'binary' cue-list as with BINCUE.
         Alternatively a 'Euclidean rhythm' may be produced by using a cuelist 
         of form '(euclid points shift)  where:
 
             points is an integer less then or equal to the -total- number of 
             subbeats. For 4 bars of 4/4 with 4 subbeats to the beat, the 
             total number of 16th notes is 64.   If points is nil the default 
             is subbeats/2 + 1.

             shift is an optional integer to shift the rhythm by shift 
             subbeats. Default 0.

      3) If :print-cue-list is true the translated cue-list is printed.

      The resulting object will appear in the project tree as a QBALL.")


(setf (documentation 'binxball 'function)
      "TODO: Documentation does not include euclid2 or current return values.

Combines XBALL with integral BINCUE binary cue-list translator.
      All arguments are identical to those for XBALL and BINCUE with the
      following exceptions:

      1) :cuefn argument is not available, the QBALL uses the BAR function.
      
      2) The :cue argument takes a 'binary' cue-list as with BINCUE.
         Alternatively a 'Euclidean rhythm' may be produced by using a cuelist 
         of form '(euclid points shift)  where:
 
             points is an integer less then or equal to the -total- number of 
             subbeats. For 4 bars of 4/4 with 4 subbeats to the beat, the 
             total number of 16th notes is 64.   If points is nil the default 
             is subbeats/2 + 1.

             shift is an optional integer to shift the rhythm by shift 
             subbeats. Default 0.

      3) If :print-cue-list is true the translated cue-list is printed.

      The resulting object will appear in the project tree as an XBALL.")

