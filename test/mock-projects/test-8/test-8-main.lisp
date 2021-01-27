;;;; CYCO mock project test-8, section order
;;;;

(version 3)
(project test-7 :bars 4
	 :project-directory (join-path *mock-project-directory* "test-7"))

(plugin general-midi)
(prune-orchestra)
(general-midi-instrument piano :channel 1)
(general-midi-instrument organ :channel 2)
(general-midi-instrument sax   :channel 3 :program 'alto-sax)
(general-midi-instrument oboe  :channel 4)
(general-midi-instrument trumpet :channel 5)
(general-midi-instrument trombone :channel 6)

(lpf alpha)
(lpf beta)
(lpf gamma)

(labels ((simplify (events)
		   (remove-if-not #'(lambda (evn) (midi-note-on-p (cdr evn)))
				  events))

	 (bar-number (time)
		     (/ time (bar-duration *project*)))
	 
	 (reduce-channels ()
		 (mapcar #'(lambda (evn)
			     (cons (car evn) (1+ (channel-index (cdr evn)))))
			 (simplify (render-project)))) )
 
  ;; test-8.1 basic section order
  ;;
  (section-order '(alpha beta gamma))
  (pass? "Basic section order, mock project test 8.1"
	 (let* ((events (reduce-channels))
		(events-1 (remove-if-not #'(lambda (q)(zerop (car q))) events))
		(events-2 (remove-if-not #'(lambda (q)(= (car q)(phrase-duration alpha))) events))
		(events-3 (remove-if-not #'(lambda (q)(= (car q)(+ (phrase-duration alpha)
								(phrase-duration beta))))
					 events)))
	   (and (every #'(lambda (q)(or (= (cdr q) 1)(= (cdr q) 2))) events-1)
		(every #'(lambda (q)(or (= (cdr q) 3)(= (cdr q) 4))) events-2)
		(every #'(lambda (q)(or (= (cdr q) 5)(= (cdr q) 6))) events-3))))
	  

  ;; test 8.2 repeat beta section
  ;;
  ;; time     0             16            24 
  ;; bar      00 01 02 03   04 05 06 07   08 09 10 11
  ;; section  A             B     B       C          
  ;;
  (clear-section-order)
  (section-order '(alpha (beta :x 2) gamma))
  (pass? "Section order with repeat, mock project test 8.2"
	 (let* ((events (reduce-channels))
		(events-a (remove-if-not #'(lambda (evn)
					     (< (car evn)(* 4 (bar-duration *project*))))
					 events))
		(events-b (remove-if-not
			   #'(lambda (evn)(let ((time (car evn))
						(bar5 (* 4 (bar-duration *project*)))
						(bar8 (* 8 (bar-duration *project*))))
					    (and (<= bar5 time)(< time bar8))))
			   events))
								  
		(events-c (remove-if-not
			   #'(lambda (evn)(let ((time (car evn))
						(bar8 (* 8 (bar-duration *project*))))
					    (>= time bar8)))
			   events)))
	   (and (every #'(lambda (q)(member (cdr q) '(1 2))) events-a)
		(every #'(lambda (q)(member (cdr q) '(3 4))) events-b)
		(every #'(lambda (q)(member (cdr q) '(5 6))) events-c))))

  ;; test 8.3 transpose section A
  ;;
  ;; time      0            16
  ;; bar       00 10 02 03  04 05 06 07
  ;; section   A1           A2
  ;;
  (clear-section-order)
  (section-order '(alpha (alpha :trans 7)))
  (pass? "Section transpose, mock project test 8.3"
	 (let* ((events (render-project))
		(events-a1 (remove-if-not #'(lambda (evn)
					      (let ((time (car evn))
						    (msg (cdr evn)))
						(and (midi-note-on-p msg)
						     (< time 16))))
					  events))
		(events-a2 (remove-if-not #'(lambda (evn)
					      (let ((time (car evn))
						    (msg (cdr evn)))
						(and (midi-note-on-p msg)
						     (>= time 16))))
					  events))
		(keys-a1 (mapcar #'(lambda (evn)(data (cdr evn) 0)) events-a1))
		(keys-a2 (mapcar #'(lambda (evn)(data (cdr evn) 0)) events-a2))
		(diff (loop for a in keys-a1
			    for b in keys-a2
			    collect (- b a))) )
	   (every #'(lambda (q)(= q 7)) diff)))

  ;; test 8.4 section inversion A
  ;;
  ;; time      0           
  ;; bar       00 10 02 03
  ;; section   A1        
  ;;
  (clear-section-order)
  (section-order '((alpha :invert c5)))
  (pass? "Section inversion, mock project test 8.4"
	 (let* ((events (remove-if-not #'(lambda (evn)(midi-note-on-p (cdr evn)))
				       (render-project)))
		(keys (keyname (mapcar #'(lambda (evn)(data (cdr evn) 0)) events))))
	   (equal keys '(c7 c4))))


  ;; test 8.5 time shift
  ;;
  ;; time    0             16+1
  ;; bar     00 01 02 03   04 05
  ;; section A1            B1
  ;;
  (clear-section-order)
  (section-order '(alpha (beta :shift 1)))
  (pass? "Section time-shift, mock project test 8.4"
	 (let* ((events (remove-if-not #'(lambda (evn)(midi-note-on-p (cdr evn)))
				       (render-project)))
		(events-b (remove-if-not #'(lambda (evn)(>= (car evn) 17)) events))
		(times (mapcar #'(lambda (evn)(car evn)) events-b))
		(keys (keyname (mapcar #'(lambda (evn)(data (cdr evn) 0)) events-b))))
	   (and (equal times '(17.0 21.0))
		(equal keys '(c3 c6))))) )
