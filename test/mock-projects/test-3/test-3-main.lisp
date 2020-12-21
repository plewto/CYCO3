;;;; CYCO mock project test-3 controllers
;;;;

(version 3)
(project test-3 :tempo 60 :bars 4
	 :project-directory (join-path *mock-project-directory* "test-3"))

(plugin general-midi)
(general-midi-instrument piano :channel 1 :program 'piano1)
(general-midi-instrument organ :channel 2 :program 'organ1)

(section alpha :bars 4)

(controllers cc1 (list piano organ)
	     :events '((:cc (1 1 1) 4 64)))

(let* ((events (render-once cc1))
       (ev1 (car events))
       (ch1 (channel-index (cdr ev1)))
       (ev2 (second events))
       (ch2 (channel-index (cdr ev2))))
  (pass? "Controllers single events with layerd instruments."
	 (and (= (length events) 2)
	      (or (= ch1 0)(= ch1 1))
	      (not (= ch1 ch2))
	      (midi-control-change-p (cdr ev1))
	      (= (data (cdr ev1) 0) 4)
	      (= (data (cdr ev1) 1) 64))))



(format t "~%~%*** BUG controllers symbolic time increment is wrong ***~%~%")

(controllers cc2 piano
	     :events '((:time (1 1 1) (2 1 1) q :value 0 127 :ctrl 4 :ramp)))

(let* ((events (render-once cc2))
       (times (mapcar #'(lambda (evn)(car evn)) events))
       (values (mapcar #'(lambda (evn)(data (cdr evn) 1)) events)))
  (format t "TIMES --> ~A~%" times)
  (pass? "Controllers ramp event"
	 (and (zerop (car values))
	      (= (final values) )
	      (zerop (car times))
	      (= (- (second times)(car times)) (beat-duration cc2))
	      )))

  
  
  

