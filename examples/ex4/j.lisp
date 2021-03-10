;;;; CYCO examples ex4
;;;;
;;;; Recaman generator
;;;; https://en.wikipedia.org/wiki/Recam%C3%A1n%27s_sequence
;;;;

(section j :bars 8 :tempo 160)

(param rgen (recaman 64 :hook #'(lambda (n) (+ 48 (rem n 24)))))

(let ((cue-list (create-cue-list :bars 8))
      (key-list (next rgen 32)))
  (dump-key-list "Recaman key-list" key-list cue-list)
  
  (qball j-recaman piano
	 :cue cue-list
	 :key (next rgen 32)
	 :dur 'q))

(->midi j)
