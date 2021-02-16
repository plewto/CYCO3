;;;; CYCO examples ex4 Recaman generator
;;;;


(section d :bars 8 :tempo 120)

(param rgen (recaman 64 :hook #'(lambda (n)
			       (+ 60 (rem n 24)))))

(format t "Recaman Generator~%")
(dotimes (i 32)
  (let ((v (internal-value rgen))
	(k (value rgen)))
    (format t "[~2d] ~3d --> ~2d ~A~%" i v k (keyname k))
    (next-1 rgen)))
(reset rgen)

(let ((cue-list '()))
  (dolist (br '(1 2 3 4 5 6 7 8))
    (dolist (bt '(1 2 3 4))
      (push (list br bt 1) cue-list)
      (push (list br bt 3) cue-list)))
  (setf cue-list (reverse cue-list))

  (qball d-recaman piano
	 :cue cue-list
	 :key (next rgen 32)
	 :dur 'q))

(->midi d)
