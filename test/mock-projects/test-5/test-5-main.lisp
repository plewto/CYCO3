;;;; CYCO mock project test-5 strummer
;;;;

(version 3)
(project test-5 :tempo 60 :bars 4
	 	 :project-directory (join-path *mock-project-directory* "test-5"))

(plugin general-midi)
(general-midi-instrument piano :channel 1 :program 'piano1)

(section alpha :bars 4)

(strummer st1 piano :bars 4
	  :events '((:time (1 1 1) :key 60 :chord [solo] :dur w+w)
		    (:time (1 2 1) :cc 1 1.0)
		    (:time (1 4 1) :bend 0.0)))
(let* ((events (render-once st1))
       (key-events (remove-if-not #'(lambda (evn)
				      (or (midi-note-on-p (cdr evn))
					  (midi-note-off-p (cdr evn))))
				  events))
       (cc-event (car (filter-message-type #'midi-control-change-p 1 events)))
       (bend-event (car (filter-message-type #'midi-pitch-bend-p 1 events))))
  (loop for event in key-events
	do (pass? "strummer test 1, simple key event"
		  (and (or (= (car event) 0)(= (car event)(* 2 (bar-duration st1))))
		       (= (data (cdr event) 0) 60))))
  (pass? "strummer test 2, simple cc event"
	 (and (= (car cc-event) (bar st1 '(1 2 1)))
	      (= (data (cdr cc-event) 0) 1)
	      (= (data (cdr cc-event) 1) 127)))
  (pass? "strummer test 3, simple bend event"
	 (let* ((low (data (cdr bend-event) 0))
		(high (data (cdr bend-event) 1))
		(bend (midi-data->bend low high)))
	   (and (= (car bend-event) (bar st1 '(1 4 1)))
		(zerop bend)))))

(strummer st2 piano :bars 4
	  :events '((:time (1 1 1) :key 60 :chord [maj] :inv 0 :oct 0 :dur s)
		    (:time (2 1 1) :key 60 :inv 1 :oct 0)
		    (:time (3 1 1) :key 60 :inv 0 :oct 1)))
(let* ((events (remove-if-not #'(lambda (evn)(midi-note-on-p (cdr evn))) (render-once st2)))
       (chord-1 (mapcar #'(lambda (evn)(data (cdr evn) 0))
			(filter-time-range st2 '(1 1 1) '(1 2 1) events)))
       (chord-2 (mapcar #'(lambda (evn)(data (cdr evn) 0))
			(filter-time-range st2 '(2 1 1) '(2 2 1) events)))
       (chord-3 (mapcar #'(lambda (evn)(data (cdr evn) 0))
			(filter-time-range st2 '(3 1 1) '(3 2 1) events))))
  (pass? "strumer inversion and octave test"
	 (and (equal (sort chord-1 #'<) '(60 64 67))
	      (equal (sort chord-2 #'<) '(64 67 72))
	      (equal (sort chord-3 #'<) '(60 64 67 72)))))


(strummer st3 piano :bars 4
	  :events '((:time (1 1 1) :key 60 :chord [maj] :strum q :dur w :end-together t))
	  )

(let* ((events (render-once st3))
       (on-events (filter-message-type #'midi-note-on-p 1 events))
       (on-times (mapcar #'(lambda (evn)(car evn)) on-events))
       (off-events (filter-message-type #'midi-note-off-p 1 events))
       (off-times (mapcar #'(lambda (evn)(car evn)) off-events)))
  (pass? "strummer test 3"
	 (and
	  (monotonic-p on-times #'<)
	  (= (- (second on-times)(car on-times))(beat-duration st3))
	  (apply #'= off-times))))
