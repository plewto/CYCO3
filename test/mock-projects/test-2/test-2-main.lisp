;;;; CYCO test mock project test-1-main.lisp  QBall
;;;;
;;;; 


(version 3)

(project test-2 :tempo 60 :bars 4 :beats 4
	 :project-directory (join-path *mock-project-directory* "test-2"))

(plugin general-midi)
(prune-orchestra)
(general-midi-instrument piano :channel 1)
(general-midi-instrument organ :channel 2)

(section alpha :bars 2)

(qball qb1 piano
       :bars 2
       :cue '((1 1 1)(1 2 1)(1 3 1))
       :key '(40 41)
       :dur 'w
       :amp 'f)

(let* ((events (render-once qb1))
       (on-events (remove-if-not #'(lambda (q)(midi-note-on-p (cdr q))) events))
       (off-events (remove-if-not #'(lambda (q)(midi-note-off-p (cdr q))) events)))
  (let ((ev1 (car on-events))
	(ev2 (second on-events))
	(ev3 (third on-events)))
    (pass? "QBALL event on-time"
	   (and (zerop (car ev1))
		(= (car ev2)(beat-duration qb1))
		(= (car ev3)(* 2 (beat-duration qb1)))))
    (pass? "QBALL event keys"
	   (and (= (data (cdr ev1) 0) 40)
		(= (data (cdr ev2) 0) 41)
		(= (data (cdr ev3) 0) 40))))
  (let ((ev1 (car off-events))
	(ev2 (second off-events))
	(ev3 (third off-events)))
    (pass? "QBALL event off-times"
	   (and (= (car ev1)(bar-duration qb1))
		(= (car ev2)(+ (bar-duration qb1)(* 1 (beat-duration qb1))))
		(= (car ev3)(+ (bar-duration qb1)(* 2 (beat-duration qb1))))))))


;; Instrument layer test
(qball qb2 (list organ piano)
       :bars 2
       :cue '((1 1 1)(1 2 1)(1 3 1)(1 4 1))
       :key '(60 61 62 63)
       :amp '(ff pp)
       :dur 'w)

(let* ((events (render-once qb2))
       (piano-events (filter-message-type #'midi-note-on-p 1 events))
       (organ-events (filter-message-type #'midi-note-on-p 2 events)))
  (loop for p in piano-events
	for r in organ-events
	do (pass? "QBALL instrument layer"
		  (let ((ptime (car p))
			(rtime (car r))
			(pmsg (cdr p))
			(rmsg (cdr r)))
		    (and (= ptime rtime)
			 (= (data pmsg 0)(data rmsg 0))
			 (= (data pmsg 1)(data rmsg 1)))))))
 
;; Instrument cycle test
(qball qb3 (cycle :of (list piano organ))
       :bars 2
       :cue '((1 1 1)(1 2 1)(1 3 1)(1 4 1))
       :key '(60 61 62 63)
       :amp '(ff pp)
       :dur 'w)

(let* ((events (render-once qb3))
       (piano-events (filter-message-type #'midi-note-on-p 1 events))
       (organ-events (filter-message-type #'midi-note-on-p 2 events))
       (piano-times '((1 1 1)(1 3 1)))
       (piano-keys '(60 62))
       (organ-times '((1 2 1)(1 4 1)))
       (organ-keys '(61 63)))
  (loop for event in piano-events
	for time in piano-times
	for key in piano-keys
	do (pass? "QBALL layer test"
		  (and (= (bar qb3 time) (car event))
		       (= key (data (cdr event) 0)))))
    (loop for event in organ-events
	for time in organ-times
	for key in organ-keys
	do (pass? "QBALL layer test"
		  (and (= (bar qb3 time) (car event))
		       (= key (data (cdr event) 0))))))
