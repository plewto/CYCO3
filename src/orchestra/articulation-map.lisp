;;;; CYCO
;;;;
;;;; An articulation map is a function of form  (lambda (dur &key time-scale))
;;;; which returns float for key-down duration.
;;;; A negative result indicates a rest
;;;;
;;;; If dur argument has the value :doc the function prints
;;;; documentation and returns +rest+
;;;; 
;;;; ISSUE: Results on unrecognized values are poorly defined.
;;;;   There are at least 3 possible outcomes:
;;;;      1) Produce an error
;;;;      2) Generate warning and return rest.
;;;;      3) Ignore and return rest.


(defun basic-articulation-map (&key (scale 1.0)(min 0)(max 1e6))
  (flet ((docfn ()
		(format t "BASIC-ARTICULATION-MAP   ")
		(format t "scale: ~A  min: ~A  max: ~A~%" scale min max)
		+rest+))
    #'(lambda (metric-expression &key (time-scale 1.0))
	(cond ((eq metric-expression :doc)
	       (docfn))
	      ((rest-p metric-expression)
	       +rest+)
	      (t (limit (* time-scale scale (metric-expression metric-expression)) min max))))))

(constant +default-articulation-map+ (basic-articulation-map))


(defun constant-articulation-map (metric-expression)
  (let ((dur (metric-expression metric-expression)))
    (flet ((docfn ()
		  (format t "CONSTANT-ARTICULATION-MAP  ~A~%" metric-expression)
		  +rest+))
      #'(lambda (m &key time-scale)
	  (dismiss time-scale)
	  (cond ((eq m :doc)
		 (docfn))
		((rest-p m)
		 +rest+)
		(t dur))))))

(defun metronome-articulation-map (&key (phrase 'e)(bar 'x)(beat 'x))
  "Creates articulation map for metronome instruments.
The map returns appropriate durations for the following 
symbols:  PHRASE, BAR and BEAT.  Unrecognized symbols are as rest." 
  (flet ((docfn ()
		(format t "METRONOME-ARTICULATION-MAP~%")
		(format t "    phrase: ~A~%" phrase)
		(format t "    bar   : ~A~%" bar)
		(format t "    beat  : ~A~%" beat)
		+rest+))
    (let ((phrase-beep-duration (metric phrase))
	  (bar-beep-duration (metric bar))
	  (default-beep-duration (metric beat)))
      #'(lambda (m &key time-scale)
	  (dismiss time-scale)
	  (cond ((eq m :doc)
		 (docfn))
		((rest-p m)
		 +rest+)
		((eq m 'phrase)
		 phrase-beep-duration)
		((eq m 'bar)
		 bar-beep-duration)
		((eq m 'beat)
		 default-beep-duration)
		(t +rest+))))))
