;;;; test-3 alpha
;;;;
;;;; Basic qball


(section alpha :bars 4)

(qball alpha-piano piano
       :cue '((1 1 1)(1 2 1)(1 3 1)(1 4 1))
       :key '(c5 e5 g5 c6)
       :dur 'w
       :amp 'ff)

(qball alpha-layer (list piano bass)
       :cue '((1 1 1))
       :key 'c5
       :dur 'w
       :amp 'ff)

(qball alpha-instrument-cycle (cycle :of (list piano bass organ))
       :cue '((1 1 1)(1 2 1)(1 3 1)(1 4 1))
       :key '(c5 e5)
       :dur 'w
       :amp (line :of '(pp mf ff)))

(let ((expected-events (list (cons 0.0 (midi-note-on 0 (keynumber 'c5) 97))
			     (cons 1.0 (midi-note-on 0 (keynumber 'e5) 97))
			     (cons 2.0 (midi-note-on 0 (keynumber 'g5) 97))
			     (cons 3.0 (midi-note-on 0 (keynumber 'c6) 97))
			     
			     (cons 4.0 (midi-note-off 0 (keynumber 'c5) 0))
			     (cons 5.0 (midi-note-off 0 (keynumber 'e5) 0))
			     (cons 6.0 (midi-note-off 0 (keynumber 'g5) 0))
			     (cons 7.0 (midi-note-off 0 (keynumber 'c6) 0)))))
 
  (pass? "alpha-piano"
	 (midi-event-list-match-p expected-events
				(render-once alpha-piano))))

(let ((expected-events (list (cons 0.0 (midi-note-on 1 (keynumber 'c5) 97))
			     (cons 0.0 (midi-note-on 0 (keynumber 'c5) 97))
			     (cons 4.0 (midi-note-off 1 (keynumber 'c5) 0))
			     (cons 4.0 (midi-note-off 0 (keynumber 'c5) 0)))))
  
  (pass? "alpha-layer"
	 (midi-event-list-match-p expected-events
				  (render-once alpha-layer))))


(let ((expected-events (list (cons 0.0 (midi-note-on 0 (keynumber 'c5) 32)) ;; pp
			     (cons 1.0 (midi-note-on 1 (keynumber 'e5) 71)) ;; mf
			     (cons 2.0 (midi-note-on 2 (keynumber 'c5) 97)) ;; ff
			     (cons 3.0 (midi-note-on 0 (keynumber 'e5) 97))
			     
			     (cons 4.0 (midi-note-off 0 (keynumber 'c5) 0))
			     (cons 5.0 (midi-note-off 1 (keynumber 'e5) 0))
			     (cons 6.0 (midi-note-off 2 (keynumber 'c5) 0))
			     (cons 7.0 (midi-note-off 0 (keynumber 'e5) 0)))))
  
  (pass? "alpha-instrument-cycle"
	 (midi-event-list-match-p expected-events
				  (render-once alpha-instrument-cycle))))


