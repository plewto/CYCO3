;;;; CYCO mock project test-3 controllers
;;;;

(version 3)
(project test-3 :tempo 120 :bars 4
	 :project-directory (join-path *mock-project-directory* "test-3"))

(plugin general-midi)
(prune-orchestra)
(general-midi-instrument piano :channel 1)
(general-midi-instrument organ :channel 2)

(section alpha :bars 4)


;; Simple single controller-event for two instruments.
;
(controllers cc1 (list piano organ)
	     :events '((:cc (1 1 1) 4 64)))

(let* ((events (render-once cc1))
       (ev1 (car events))
       (ch1 (channel-index (cdr ev1)))
       (ev2 (second events))
       (ch2 (channel-index (cdr ev2))))
  (pass? "Controllers single events with layerd instruments.  Test 3.1"
	 (and (= (length events) 2)
	      (or (= ch1 0)(= ch1 1))
	      (not (= ch1 ch2))
	      (midi-control-change-p (cdr ev1))
	      (= (data (cdr ev1) 0) 4)
	      (= (data (cdr ev1) 1) 64))))



;; Curve event
;;
(controllers cc2 piano
	     :events '((:time (1 1 1) (2 1 1) q :value 0 127 :ctrl 4 :ramp)))

(let* ((events (render-once cc2))
       (times (mapcar #'(lambda (evn)(car evn)) events))
       (values (mapcar #'(lambda (evn)(data (cdr evn) 1)) events))
       (time-increment (- (second times)(first times)))
       (expected-increment (beat-duration cc2)))
  (pass? "Controllers ramp event.  Test 3.2"
	 (and
	  (monotonic-p times)
	  (zerop (car values))
	  (= (final values) 127)
	  (monotonic-p values)
	  (zerop (car times))
	  (= time-increment expected-increment))))



;; Controller ramp starting at non-zero-time
;;
(controllers cc3 piano
	     :events '((:time (2 1 1)(3 1 1) e :value 0 127 :ctrl 4 :ramp)))

(let* ((events (render-once cc3))
       (times (mapcar #'(lambda (evn)(car evn)) events))
       (values (mapcar #'(lambda (evn)(data (cdr evn) 1)) events))
       (expected-start-time (bar cc3 '(2 1 1)))
       (actual-start-time (car times)))
  (pass? "Controllers ramp start time test, Test 3.3"
	 (and actual-start-time
	      (= actual-start-time expected-start-time)
	      (monotonic-p times)
	      (monotonic-p values))))

;; Pressure ramp
;;   ignore cc events between (1 1 1) and (2 1 1)
;;   check that < start-time equals previous end-time.
;;   pressure values should default to range 10..64
;;
(controllers pr1 piano
	     :events '((:time (1 1 1)(2 1 1) q :value 10 64 :ctrl 4 :ramp)
		       (:time < (3 1 1) q :ctrl pressure :ramp)))
  
(let* ((events (filter-message-type #'midi-channel-pressure-p 1 (render-once pr1)))
       (times (mapcar #'(lambda (evn)(car evn)) events))
       (values (mapcar #'(lambda (evn)(data (cdr evn) 0)) events)))
  (pass? "Pressure ramp events. Test 3.4"
	 (and (monotonic-p values)
	      (= (car values) 10)
	      (= (final values) 64)
	      (monotonic-p times)
	      (= (car times)(bar pr1 '(2 1 1)))
	      (= (final times)(bar pr1 '(3 1 1))))))




;; Single pitch-bend event on two channels.
;;
(bender b1 (list piano organ)
	:events'((:bend (1 1 1) 1.0)))

(let* ((events (render-once b1))
       (times (mapcar #'(lambda (evn)(car evn)) events))
       (values (mapcar #'(lambda (evn)
			   (let* ((msg (cdr evn))
				  (lsb (data msg 0))
				  (msb (data msg 1)))
			     (midi-data->bend lsb msb)))
		       events)))
  (pass? "Single bender event. Test 3.5"
	 (and (zerop (car times))
	      (= 1.0 (car values))
	      (= (length times)(length values) 2))))

;; Bender curve
;;
(bender b2 piano
	:events '((:time (2 1 1)(3 1 1) :value -1.0 1.0 :ramp)))

(let* ((events (render-once b2))
       (times (mapcar #'(lambda (evn)(car evn)) events))
       (values (mapcar #'(lambda (evn)(let* ((msg (cdr evn))
					     (lsb (data msg 0))
					     (msb (data msg 1)))
					(midi-data->bend lsb msb)))
		       events)))
  (pass? "Bender curve. Test 3.6"
	 (and (monotonic-p times)
	      (monotonic-p values)
	      (= (car times) 2.0)
	      (= (car values) -1.0))))



