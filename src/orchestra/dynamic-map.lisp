;;;; CYCO
;;;;
;;;; A dynamic-map is a function which maps dynamic values to
;;;; 'normalized' amplitude between 0 and 1 inclusive or +REST+.
;;;; If the maps argument is :DOC it prints documentation and
;;;; returns +REST+
;;;;

(defun basic-dynamic-map (&key (scale 1)(min 0.0)(max 1.0))
  "Creates default dynamic-map (lambda dynamic) --> amplitude
The amplitude may be optionally scaled and limited.
If the map argument is :DOC, the function prints documentation 
and returns +REST+"
  (flet ((docfn ()
		(format t ";;BASIC-DYNAMIC-MAP")
		(format t ";;    scale: ~A   min: ~A   max: ~A~%" scale min max)
		0))
    #'(lambda (a)
	(let ((rs (cond ((eq a :doc)
			 (docfn))
			((rest-p a)
			 +rest+)
			(t
			 (limit (* scale (dynamic a)) min max)))))
	  rs))))

(defun metronome-dynamic-map (&key (phrase 'ffff)(bar 'fff)(beat 'f))
  "Creates dynamic-map for use with metronome instruments.
Recognized arguments (PHRASE, BAR and BEAT) are converted to 
appropriate amplitude values.  The map returns +REST+ for unrecognized
arguments."
  (flet ((docfn ()
		(format t ";; METRONOME-DYNAMIC-MAP~%")
		(format t ";;    phrase: ~A~%" phrase)
		(format t ";;    bar: ~A~%" bar)
		(format t ";;    beat: ~A~%" beat)
		0.0))
    (let ((phrase-amplitude (dynamic phrase))
	  (bar-amplitude (dynamic bar))
	  (beat-amplitude (dynamic beat)))
      #'(lambda (a)
	  (cond ((eq a :doc)
		 (docfn))
		((eq a 'phrase)
		 phrase-amplitude)
		((eq a 'bar)
		 bar-amplitude)
		((eq a 'beat)
		 beat-amplitude)
		(t +REST+))))))


(constant +default-dynamic-map+ (basic-dynamic-map))
	      
