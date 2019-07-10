

(defchord '[foo] '(0 1 2 3) "Defchord test")


(pass? (and (chord-model-p *chord-table*)
	    (not (chord-model-p t)))
       "chord-model-p")

(pass? (same-thing-p (chord-template *chord-table* '[foo] nil) '(0 1 2 3)) "chord-template")


(let* ((template '(0 1 2)))
  (pass? (and (same-thing-p (chord-inversion template 0 :add-octave 0) template)
	      (same-thing-p (chord-inversion template 1 :add-octave 0) '(1 2 12))
	      (same-thing-p (chord-inversion template -1 :add-octave 0) '(2 0 1))
	      (same-thing-p (chord-inversion template 1 :add-octave 1) '(1 2 12 13)))
	 "chord-inversion"))
  
(pass? (and (defines-chord-p *chord-table* '[foo])
	    (not (defines-chord-p *chord-table* '[dog])))
       "defines-chord-p")

