;;;; CYCO example ex4 e
;;;; Hailstone generator
;;;;

(section e :bars 8 :tempo 120)

(param hs (hailstone 33 :hook #'(lambda (n)
				  (+ 36 (rem (1- n) 36)))))

(let ((cv (internal-value hs))
      (counter 0))
  (while (not (= cv 1))
    (format t "[~2d] ~3d --> ~2d ~A~%" counter cv (value hs)(keyname (value hs)))
    (next-1 hs)
    (setf cv (internal-value hs))
    (setf counter (1+ counter)))
  (setf cv (internal-value hs))
  (format t "[~2d] ~3d --> ~2d ~A~%" counter cv (value hs)(keyname (value hs))))
		     
(reset hs)

(let ((cue-list '()))
  (dolist (br '(1 2 3 4 5 6 7 8))
    (dolist (bt '(1 2 3 4))
      (push (list br bt 1) cue-list)
      (push (list br bt 3) cue-list)))
  (setf cue-list (reverse cue-list))

  (qball d-recaman piano
	 :cue cue-list
	 :key (next hs 26)
	 :dur 'q))


(->midi e)
