;;;; cyco composition binball.lisp
;;;;
;;;; Defines combined structures for BINCUE+QBALL and BINCUE+XBALL.

(in-package :cyco)


(defun pprint-cue-list (name cue-list)
  (let ((current (caar cue-list)))
    (format t "~A CUE-LIST:~%" name)
    (dolist (q cue-list)
      (if (not (eq (car q) current))
	  (progn
	    (format t "~%")
	    (setf current (car q))))
      (format t "~A " q))
    (format t "~%")))

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
	    (bar-cue-list (if (eq (car ,cue) 'euclid)
			      (let ((points (or (second ,cue) (1+ (/ (cue-points tsig) 2))))
				    (shift (or (third ,cue) 0)))
				(euclid->binary-cuelist points :shift shift :timesig tsig :use-subbeats ,use-subbeats))
			    (bincue-translate bincue ,cue)))
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
			    :cue bar-cue-list
			    :key (or ,key '(60))
			    :dur (or ,dur '(q))
			    :amp (or ,amp '(fff))
			    :reset-on-repeat ,reset-on-repeat
			    :remarks (->string (or ,remarks "")))))
       (if ,print-cue-list (pprint-cue-list (name qb) bar-cue-list))
       (defparameter ,name qb)
       qb)))

		      
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
	     (bar-cue-list (if (eq (car ,cue) 'euclid)
			      (let ((points (or (second ,cue) (1+ (/ (cue-points tsig) 2))))
				    (shift (or (third ,cue) 0)))
				(euclid->binary-cuelist points :shift shift :timesig tsig :use-subbeats ,use-subbeats))
			     (bincue-translate bincue ,cue)))
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
			    :cue bar-cue-list
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
       (if ,print-cue-list (pprint-cue-list (name xb) bar-cue-list))
       (defparameter ,name xb)
       xb)))
				 
       
(setf (documentation 'binball 'function)
      "Combines QBALL with integral BINCUE binary cue-list translator.
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
      "Combines XBALL with integral BINCUE binary cue-list translator.
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

