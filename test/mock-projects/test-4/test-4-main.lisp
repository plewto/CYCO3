;;;; CYCO mock project test-4 simple-part
;;;;

(version  3)
(project test-4 :tempo 60 :bars 4
	 :project-directory (join-path *mock-project-directory* "test-4"))

(plugin general-midi)
(general-midi-instrument piano :channel 1 :program 'piano1)
(general-midi-instrument organ :channel 2 :program 'organ1)

(section alpha :bars 4)

;;; Basics
;;;

(simple-part s1 (list piano organ)
	     :bars 4
	     :events '((:time (1 1 1) :key 60 :chord [solo] :dur w)))
		     
(let* ((events (render-once s1))
       (on-events-1 (filter-message-type #'midi-note-on-p 1 events))
       (on-events-2 (filter-message-type #'midi-note-on-p 2 events))
       (off-events-1 (filter-message-type #'midi-note-off-p 1 events))
       (off-events-2 (filter-message-type #'midi-note-off-p 2 events)))
  (loop for on1 in on-events-1
	for on2 in on-events-2
	do (pass? "Simple-part note on events"
		  (and (= (car on1)(car on2) 0)
		       (= (data (cdr on1) 0)(data (cdr on2) 0)))))
  (loop for off1 in off-events-1
	for off2 in off-events-2
	do (pass? "Simple-part note off events"
		  (and (= (car off1)(car off2)(bar-duration s1))
		       (= (data (cdr off1) 0)(data (cdr off2) 0))))))

;;; Chords 
;;;

(simple-part s2 piano
	     :bars 4
	     :events '((:time (1 2 1) :key 40 :chord [maj] :inv 0 :oct 0 :dur w)))
(let* ((events (render-once s2))
       (times (mapcar #'(lambda (evn)(car evn)) events))
       (keys (remove-duplicates (sort (mapcar #'(lambda (evn)(data (cdr evn) 0)) events) #'<))))
  (pass? "Simple-part chord test 1"
	 (and (length times) 6))
  (pass? "Simple-part chord test 2"
	 (every #'(lambda (q)(or (= q (bar s2 '(1 2 1)))
				 (= q (bar s2 '(2 2 1)))))
		times))
  (pass? "Simple-part chord test 3"
	 (equal keys '(40 44 47))))

  
(simple-part s3 piano
	     :bars 4
	     :events '((:time (1 1 1) :key 40 :chord [maj] :inv 1 :oct 0)
		       (:time (2 1 1) :key 60 :chord [maj] :inv 0 :oct 1)))
(let* ((events (render-once s3))
       (inv-1 (filter-time-range s3 '(1 1 1) '(1 2 1) events))
       (keys-inv-1 (mapcar #'(lambda (evn)(data (cdr evn) 0)) inv-1))
       (oct-1 (filter-time-range s3 '(2 1 1) '(2 2 1) events))
       (keys-oct-1 (mapcar #'(lambda (evn)(data (cdr evn) 0)) oct-1)))
  (pass? "Simple-part chord test 4"
	 (equal (remove-duplicates (sort keys-inv-1 #'<)) '(44 47 52)))
  (pass? "Simple-part chord test 5"
   	 (equal (remove-duplicates (sort keys-oct-1 #'<)) '(60 64 67 72))))
	     

;;; Controlers, pressure and bend.
;;;

(simple-part s3 organ
	     :bars 4 :events '((:time (1 1 1) :cc 2 0)
			       (:time (1 1 3) :cc foot 64)
			       (:time (1 2 1) :pressure 64)
			       (:time (1 3 1) :bend 0.0)
			       (:time (1 4 1) :program default)))
(let* ((events (render-once s3))
       (msg1 (cdr (first events)))
       (ctrl-1 (data msg1 0))
       (cc-1 (data msg1 1))
       (msg2 (cdr (second events)))
       (ctrl-2 (data msg2 0))
       (cc-2 (data msg2 1))
       (msg3 (cdr (third events)))
       (pressure (data msg3 0))
       (msg4 (cdr (fourth events)))
       (low (data msg4 0))
       (high (data msg4 1))
       (msg5 (cdr (fifth events)))
       (program (data msg5 0)))
  (pass? "Simple-part test 6"
	 (and (= ctrl-1 2)
	      (= cc-1 0)
	      (= ctrl-2 4)
	      (= cc-2 64)
	      (= pressure 64)
	      (zerop (midi-data->bend low high))
	      (= program (general-midi-program 'organ1)))))

      
