;;;; CYCO
;;;;

(defun basic-dynamic-map (&key (scale 1)(min 0.0)(max 1.0))
  (flet ((docfn ()
		(format t "BASIC-DYNAMIC-MAP")
		(format t "    scale: ~A   min: ~A   max: ~A~%" scale min max)
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
  (flet ((docfn ()
		(format t "METRONOME-DYNAMIC-MAP~%")
		(format t "    phrase: ~A~%" phrase)
		(format t "       bar: ~A~%" bar)
		(format t "      beat: ~A~%" beat)
		0.0))
    (let ((dphrase (dynamic phrase))
	  (dbar (dynamic bar))
	  (dbeat (dynamic beat)))
      #'(lambda (a)
	  (cond ((eq a :doc)
		 (docfn))
		((eq a 'phrase)
		 dphrase)
		((eq a 'bar)
		 dbar)
		((eq a 'beat)
		 dbeat)
		(t 0.0))))))


(constant +default-dynamic-map+ (basic-dynamic-map))
	      



