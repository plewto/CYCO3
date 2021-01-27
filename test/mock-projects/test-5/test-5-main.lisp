;;;; CYCO mock project test-5 strummer
;;;;

(version 3)
(project test-5 :tempo 60 :bars 4
	 	 :project-directory (join-path *mock-project-directory* "test-5"))

(plugin general-midi)
(plugin guitar-chords)
(prune-orchestra)
(general-midi-instrument piano :channel 1)
(general-midi-instrument organ :channel 1)

(section alpha :bars 4)


;; controllers
;;
(strummer st01 piano :bars 4
	  :events '((:time (1 1 1) :cc 1 64)
		    (:time (2 1 1) :cc foot 65)))

(let* ((events (render-once st01))
       (times (mapcar #'(lambda (evn)(car evn)) events))
       (controllers (mapcar #'(lambda (evn)(data (cdr evn) 0)) events))
       (values (mapcar #'(lambda (evn)(data (cdr evn) 1)) events)))
  (pass? "controllers, Strummer test 1"
	 (and
	  (equal times (list (bar st01 '(1 1 1))(bar st01 '(2 1 1))))
	  (equal controllers (list 1 (get-controller-number 'foot)))
	  (equal values (list 64 65)))))
	      

;; pitch bend
;;
(strummer st02 piano :bars 4
	  :events '((:time (1 1 1) :bend -1.0)
		    (:time (2 1 1) :bend 0.0)
		    (:time (3 1 1) :bend +1.0)))
(let* ((events (render-once st02))
       (values (mapcar #'(lambda (evn)
			   (let* ((msg (cdr evn))
				  (low (data msg 0))
				  (high (data msg 1)))
			     (midi-data->bend low high)))
		       events))
       (diff (loop for v in values
		   for u in '(-1.0 0.0 1.0)
		   collect (- v u))))
  ;; Passes SBCL  Fails CLISP
  ;; (pass? "bend, strummer test 2"
  ;; 	 (equal values '(-1.0 0.0 1.0))))
  (pass? "bend, strummer test 2"
	 (every #'zerop diff)))

  

;; programs
;;

(strummer st03 organ :bars 4
	  :events '((:time (1 1 1) :program default)
		    (:time (2 1 1) :program 19)))
(let* ((events (render-once st03))
       (values (mapcar #'(lambda (evn)(data (cdr evn) 0)) events)))
  (pass? "programs, strummer test 3"
	 (equal values (list (general-midi-program 'organ) 19))))


;; simple chords
;;

(strummer st04 piano :bars 4
	  :events '((:time (1 1 1) :key 60 :chord [maj] :dur q)
		    (:time (2 1 1) :key 60 :chord (0 1 2))))
(let* ((events (render-once st04))
       (on-1 (mapcar #'(lambda (evn)(data (cdr evn) 0))
		     (filter-time-range st04 '(1 1 1) '(1 1 2) events)))
       (on-2 (mapcar #'(lambda (evn)(data (cdr evn) 0))
		     (filter-time-range st04 '(2 1 1) '(2 1 2) events))))
  (pass? "simple chords, strummer test 4"
	 (and
	  (equal '(60 64 67) (sort on-1 #'<))
	  (equal '(60 61 62) (sort on-2 #'<)))))

;; simple chord using guitar plugin
;;



(strummer st05 piano :bars 4
	  :chord-model *guitar-chord-model*
	  :events '((:time (1 1 1) :key g1 :chord [maj] :dur q)
		    (:time (2 1 1) :key a6  :chord (60 64 67))))
(let* ((events (render-once st05))
       (g-chord (sort (remove-duplicates
		       (pitch-class (mapcar #'(lambda (evn)(data (cdr evn) 0))
					    (filter-time-range st04 '(1 1 1) '(1 1 2) events))))
		      #'<))
       (c-chord (mapcar #'(lambda (evn)(data (cdr evn) 0))
			(filter-time-range st04 '(2 1 1) '(2 1 2) events))))
  (pass? "absolute chords, strummer test 5"
	 (and (equal g-chord '(2 7 11))
	      (equal c-chord '(60 64 67)))))

;; inversion & octave
;;

(strummer st06 piano :bars 4
	  :events '((:time (1 1 1) :key 60 :chord [maj] :inv 0 :oct 0 :dur q)
		    (:time (2 1 1) :key 60 :chord [maj] :inv 1 :oct 0)
		    (:time (3 1 1) :key 60 :chord [maj] :inv 1 :oct 1)))

(flet ((filter (start-time end-time event-list)
	       (let ((events (filter-time-range st06 start-time end-time event-list)))
		 (sort (mapcar #'(lambda (evn)(data (cdr evn) 0)) events) #'<))))
  (let* ((events (render-once st06))
	 (chord-1 (filter '(1 1 1) '(1 1 2) events))
	 (chord-2 (filter '(2 1 1) '(2 1 2) events))
	 (chord-3 (filter '(3 1 1) '(3 1 2) events)))
    (pass? "inversion & octave, strummer test 6"
	   (and (equal chord-1 '(60 64 67))
		(equal chord-2 '(64 67 72))
		(equal chord-3 '(64 67 72 76))))))

;; direction
;;

(strummer st07 piano :bars 4
	  :events '((:time (1 1 1) :key 60 :chord [maj] :direction up :dur q)
		    (:time (2 1 1) :key 60 :direction down)))
(let* ((events (render-once st07))
       (chord-1 (mapcar #'(lambda (evn)(data (cdr evn) 0))
			(filter-time-range st07 '(1 1 1) '(1 1 2) events)))
       
       (chord-2 (mapcar #'(lambda (evn)(data (cdr evn) 0))
			(filter-time-range st07 '(2 1 1) '(2 1 2) events))))
  (pass? "strum direction, strummer test 7"
	 (equal chord-1 (reverse chord-2))))


;; strum delay
;;

(strummer st08 piano :bars 4
	  :events '((:time (1 1 1) :key 60 :chord [maj] :strum s :dur q :end-together nil)))
(let* ((events (render-once st08))
       (on-times (mapcar #'(lambda (evn)(car evn))
			 (remove-if-not #'(lambda (evn)(midi-note-on-p (cdr evn))) events)))

       (off-times (mapcar #'(lambda (evn)(car evn))
			 (remove-if-not #'(lambda (evn)(midi-note-off-p (cdr evn))) events)))
       (expected-delay (* 0.25 (beat-duration st08))))
  (pass? "strum delay, strummer test 8"
	 (and (monotonic-p on-times)
	      (= (- (second on-times)(first on-times)) expected-delay)
	      (monotonic-p off-times)
	      (= (- (second off-times)(first off-times)) expected-delay))))

;; end-together
;;

(strummer st09 piano :bars 4
	  :events '((:time (1 1 1) :key 60 :chord [maj] :strum s :dur q :end-together yes)))
(let* ((events (render-once st09))
       (off-times (mapcar #'(lambda (evn)(car evn))
			 (remove-if-not #'(lambda (evn)(midi-note-off-p (cdr evn))) events))))
  (apply #'= off-times))
       

;; strum delay acceleration
;;

(strummer st10 piano :bars 4
	  :events '((:time (1 1 1) :key 60 :chord (0 1 2 3 4) :strum s :strum* 1.01 :dur 2*w)))

(let* ((events (render-once st10))
       (times (mapcar #'(lambda (evn)(car evn))
		      (filter-time-range st10 '(1 1 1) '(2 1 1) events)))
       (t0 (first times))
       (t1 (second times))
       (t2 (third times))
       (d1 (abs (- t1 t0)))
       (d2 (abs (- t2 t1))))
  (pass? "strum acceleration, strummer test 10"
	 (not (= d1 d2))))

			     
;; strum amp scale
;;

(strummer st11 piano :bars 4
	  :events '((:time (1 1 1) :key 60 :chord (0 1 2 3 4) :strum s :amp mp :amp* 1.1)))
(let* ((events (render-once st11))
       (amps (mapcar #'(lambda (evn)(data (cdr evn) 1))
		     (remove-if-not #'(lambda (evn)(midi-note-on-p (cdr evn))) events))))
  (pass? "chord amp scale, strummer test 11"
	 (monotonic-p amps #'<)))

;; amp blur
;;

(strummer st12 piano :bars 4
	  :events '((:time (1 1 1) :key 60 :amp mp :amp-blur 1.0)
		    (:time (1 2 1) :key 60)))
(let*  ((events (render-once st11))
	(amps (mapcar #'(lambda (evn)(data (cdr evn) 1))
		      (remove-if-not #'(lambda (evn)(midi-note-on-p (cdr evn))) events))))
  (pass? "amp blur, strummer test 12\n
NOTE: Due to the random nature of amp blurr, there is a small chance this test will fail."
	 (not (apply #'= amps))))


;; basic amp level
;;

(strummer st13 piano :bars 4
	  :events '((:time (1 1 1) :key 60 :amp pppp)
		    (:time (2 1 1) :key 60 :amp ffff)))
(let*  ((events (render-once st13))
	(amps (mapcar #'(lambda (evn)(data (cdr evn) 1))
		      (remove-if-not #'(lambda (evn)(midi-note-on-p (cdr evn))) events))))
  (pass? "amp level, strummer test 13"
	 (and (< (car amps) 8)
	      (< 112 (second amps)))))

;; amp-limit
;; 

(strummer st14 piano :bars 4
	  :events '((:amp-limits pp ff)
		    (:time (1 1 1) :key 60 :amp pppp)
		    (:time (1 1 1) :key 60 :amp ppp)
		    (:time (1 1 1) :key 60 :amp pp)
		    (:time (1 1 1) :key 60 :amp p)
		    (:time (1 1 1) :key 60 :amp mp)
		    (:time (1 1 1) :key 60 :amp mf)
		    (:time (1 1 1) :key 60 :amp f)
		    (:time (1 1 1) :key 60 :amp ff)
		    (:time (1 1 1) :key 60 :amp fff)
		    (:time (1 1 1) :key 60 :amp ffff)))
(let*  ((events (render-once st14))
	(amps (mapcar #'(lambda (evn)(data (cdr evn) 1))
		      (remove-if-not #'(lambda (evn)(midi-note-on-p (cdr evn))) events))))
  (pass? "amp limits, strummer test 14"
	 (every #'(lambda (amp)(and (<= (dynamic->velocity (dynamic 'pp)) amp)
				    (<= amp (dynamic->velocity (dynamic 'ff)))))
		amps)))

;; amp-pattern
;;
(strummer st15 piano :bars 4
	  :events '((:time (1 1 1) :key 60 :amp (pp mp ff) :dur 3*w)
		    (:time (1 1 2) :key 61)
		    (:time (1 1 3) :key 62)
		    (:time (1 2 1) :key 60)
		    (:time (1 2 2) :key 61)
		    (:time (1 2 3) :key 62)
		    (:time (1 3 1) :key 60)
		    (:time (1 3 2) :key 61)
		    (:time (1 3 3) :key 62)))

(let* ((events (remove-if-not #'(lambda (evn)(midi-note-on-p (cdr evn)))(render-once st15)))
	 (e60 (remove-if-not #'(lambda (evn)(= (data (cdr evn) 0) 60)) events))
	 (e61 (remove-if-not #'(lambda (evn)(= (data (cdr evn) 0) 61)) events))
	 (e62 (remove-if-not #'(lambda (evn)(= (data (cdr evn) 0) 62)) events))
	 (a60 (mapcar #'(lambda (evn)(data (cdr evn) 1)) e60))
	 (a61 (mapcar #'(lambda (evn)(data (cdr evn) 1)) e61))
	 (a62 (mapcar #'(lambda (evn)(data (cdr evn) 1)) e62)))
  (pass? "amp pattern, strummer test 15"
	 (and (not (= (car a60)(car a61)))
	      (not (= (car a60)(car a62)))
	      (not (= (car a61)(car a62)))
	      (apply #'= a60)
	      (apply #'= a61)
	      (apply #'= a62))))
       
       
;; crescendo
;;
(strummer st16 piano :bars 4
	  :events '((:time (1 1 1) :key 60 :cres pp ff 4)
		    (:time (1 1 2) :key 61)
		    (:time (1 1 3) :key 62)
		    (:time (1 1 4) :key 63)
		    (:time (2 1 1) :key 64)
		    (:time (2 1 2) :key 65)))
(let* ((events (remove-if-not #'(lambda (evn)(midi-note-on-p (cdr evn)))
			      (render-once st16)))
       (amps (mapcar #'(lambda (evn)(data (cdr evn) 1)) events)))
  (pass? "crescendo, strummer test 16"
	 (and (monotonic-p amps #'<=)
	      (not (= (fourth amps)(fifth amps)))
	      (= (fifth amps)(sixth amps)))))


;; duration 
;;
(strummer st17 piano :bars 4
	  :events '((:time (1 1 1) :key 60 :dur s)
		    (:time (1 1 1) :key 61 :dur e)
		    (:time (1 1 1) :key 62 :dur h)
		    (:time (1 1 1) :key 70 :dur 6)))

(let ((events (remove-if-not
	       #'(lambda (evn)(midi-note-off-p (cdr evn)))
	       (render-once st17)))
      (beat (beat-duration st17)))
  (labels ((event-time (key-number)
		       (let ((sub-events
			      (remove-if-not
			       #'(lambda (evn)(= (data (cdr evn) 0) key-number))
			       events)))
			 (car (car sub-events)))))
    (pass? "duration, strummer test 17"
	   (and (= (event-time 60)(* 0.25 beat))
		(= (event-time 61)(* 0.50 beat))
		(= (event-time 62)(* 2 beat))
		(= (event-time 70) 6)))))

;; grace notes
;;
(strummer st18 piano :bars 4
	  :events '((:time (1 1 1) :key 30 :dur w :amp ffff)
		    (:grace-amp* 0.5 :grace-duration s :grace-delay q :grace 72)))
(let* ((events (remove-if #'(lambda (evn)(= (data (cdr evn) 0) 30))
			  (render-once st18)))
       (base-amp (dynamic->velocity (dynamic 'ffff)))
       (on-event (car events))
       (off-event (second events)))
  (pass? "grace-notes, strummer test 18"
	 (and (= (- (car off-event)(car on-event))(* 0.25 (beat-duration st18)))
	      (= (car on-event)(beat-duration st18))
	      (~= (data (cdr on-event) 1)(* 0.5 base-amp) 2))))
	   
