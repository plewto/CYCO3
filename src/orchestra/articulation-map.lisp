;;;; CYCO3 src/orchestra/articulation-map
;;;;
;;;; An articulation map is a function of form  (lambda (dy &key time-scale))
;;;; which returns float for key-down duration.
;;;; A negative result indicates a rest
;;;;
;;;; If dy argument has the value :doc the function prints
;;;; documentation and returns +rest+
;;;; 


(defun basic-articulation-map (&key (scale 1.0)(min 0)(max 1e6))
  (flet ((docfn ()
		(format t "BASIC-ARTICULATION-MAP   ")
		(format t "scale: ~A  min: ~A  max: ~A~%" scale min max)
		+rest+))
    #'(lambda (m &key (time-scale 1.0))
	(cond ((eq m :doc)
	       (docfn))
	      ((rest-p m)
	       +rest+)
	      (t (limit (* time-scale scale (metric-expression m)) min max))))))

(constant +default-articulation-map+ (basic-articulation-map))


(defun constant-articulation-map (mxp)
  (let ((dur (metric-expression mxp)))
    (flet ((docfn ()
		  (format t "CONSTANT-ARTICULATION-MAP  ~A~%" mxp)
		  +rest+))
      #'(lambda (m &key time-scale)
	  (dismiss time-scale)
	  (cond ((eq m :doc)
		 (docfn))
		((rest-p m)
		 +rest+)
		(t dur))))))

(defun metronome-articulation-map (&key (phrase 'e)(bar 'x)(beat 'x))
  (flet ((docfn ()
		(format t "METRONOME-ARTICULATION-MAP~%")
		(format t "    phrase: ~A~%" phrase)
		(format t "    bar   : ~A~%" bar)
		(format t "    beat  : ~A~%" beat)
		+rest+))
    (let ((dphrase (metric phrase))
	  (dbar (metric bar))
	  (dbeat (metric beat)))
      #'(lambda (m &key time-scale)
	  (dismiss time-scale)
	  (cond ((eq m :doc)
		 (docfn))
		((rest-p m)
		 +rest+)
		((eq m 'phrase)
		 dphrase)
		((eq m 'bar)
		 dbar)
		((eq m 'beat)
		 dbeat)
		(t dbeat))))))

