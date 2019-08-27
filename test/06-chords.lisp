;;;; test 06-chords
;;;;

(defchord '[foo] '(0 1 2 3) "Defchord test")

(pass? "chord-model-p"
       (and (chord-model-p *chord-table*)
	    (not (chord-model-p 13))))

(pass? "defchord"
       (equal (chord-template *chord-table* '[foo] nil)
	      '(0 1 2 3)))

(let ((template '(0 1 2)))
  (pass? "chord-inversion"
	 (and (equal (chord-inversion template  0 :add-octave 0) template)
	      (equal (chord-inversion template  1 :add-octave 0) '(1 2 12))
	      (equal (chord-inversion template -1 :add-octave 0) '(2 0 1))
	      (equal (chord-inversion template  1 :add-octave 1) '(1 2 12 13)))))

(pass? "defines-chord-p"
       (and (defines-chord-p *chord-table* '[foo])
	    (not (defines-chord-p *chord-table* '[dog]))))
  
