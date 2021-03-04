;;;; CYCO example ex4 e
;;;;
;;;; Hailstone generator
;;;; https://en.wikipedia.org/wiki/Collatz_conjecture
;;;;

;;;; The initial seed value has a strong influence over the generated sequence.
;;;; The sequence will (always ?) eventually fall into a three-note repeating
;;;; pattern. 

(section e :bars 8 :tempo 120)

(param hs (hailstone 33 :hook #'(lambda (n)(+ 36 (rem (1- n) 36)))))

(let ((cue-list (create-cue-list :bars 8))
      (key-list (next hs 26)))

  (dump-key-list "Hailstone key-list" key-list cue-list)
 
  (qball e-recaman piano
	 :cue cue-list
	 :key key-list
	 :dur 'q))


(->midi e)

