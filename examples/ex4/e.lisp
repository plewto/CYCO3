;;;; CYCO example ex4 e
;;;;
;;;; Hailstone generator
;;;;

(section e :bars 8 :tempo 120)

(param hs (hailstone 33 :hook #'(lambda (n)(+ 36 (rem (1- n) 36)))))

(let ((cue-list (create-cue-list :bars 8))
      (key-list (next hs 26)))

  (dump-key-list "Hailstone key-list" key-list cue-list)
 
  (qball d-recaman piano
	 :cue cue-list
	 :key key-list
	 :dur 'q))


(->midi e)

