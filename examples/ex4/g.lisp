;;;; CYCO examples g
;;;;
;;;; Example g uses the SAMPLE-AND-HOLD pattern and PALINDROME function
;;;; to create string harmony below a simple piano part.
;;;;

(section g :bars 8 :tempo 120)

(let* ((piano-cue-list (create-cue-list :bars 2))
       (motif (lfo :curve (keynumber '(c5 ef5 bf5 g5 c6))))
       (piano-keys (next motif 5))  

       ;; Slowglass extends the motif.
       (slow-cue-list (create-cue-list :bars 4 :beats 4))
       (slow-motif (slowglass (clone motif) :n 3))
       (slow-keys (transpose (next slow-motif 16) -24)))

  ;; Basic piano part
  ;; 
  (qball g-piano piano
	 :bars 2
	 :reset-on-repeat nil
	 :cue piano-cue-list
	 :key piano-keys
	 :dur 'q
	 :amp 'mf)

  ;; Generated strings part.
  ;; Actual string instruments cycle every third note.
  ;; 
  (qball g-strings (cycle :of (list strings high-strings high-strings))
	 :bars 4
	 :cue slow-cue-list
	 :key (palindrome slow-keys :elide :end)
	 :dur 'q.
	 :amp 'mf))


(->midi g)