;;;; CYCO mock project test-6 transformer
;;;;

(version 3)
(project test-6 :tempo 60 :bars 4
	 :project-directory (join-path *mock-project-directory* "test-6"))

(plugin general-midi)
(general-midi-instrument piano :channel 1 :program 'piano1)
(general-midi-instrument organ :channel 2 :program 'organ1)

(section alpha :bars 4)


;; identify
;;

(simple-part sa1 piano
	     :bars 4
	     :events '((:time (1 1 1) :key 60)))

(transformer tx1 sa1)

(let ((ev1 (render-once sa1))
      (ev2 (render-once tx1)))
  (loop for a in ev1
	for b in ev2
	do (pass? "identity, transformer test 1"
		  (and (= (car a)(car b))
		       (= (channel-index (cdr a))(channel-index (cdr b)))
		       (= (data (cdr a) 0)(data (cdr b) 0))))))


;; time filter
;;

(simple-part sa2 piano
	     :bars 4
	     :events '((:time (1 1 1) :key 60 :dur q)
		       (:time (2 1 1) :key 61 :dur q)
		       (:time (3 1 1) :key 62 :dur q)
		       (:time (4 1 1) :key 63 :dur q)))

(transformer tx2 sa2
	     :filter #'(lambda (part event)
			 (<= (car event)(bar part '(2 1 1)))))

(let* ((events (render-once tx2))
       (times (sort (mapcar #'(lambda (evn)(car evn)) events) #'<)))
  (pass? "time filter, transformer test 2"
	 (every #'(lambda (value)(<= value (bar tx2 '(2 1 1)))) times)))



;; shift-channel
;;

(simple-part sa3 piano
	     :bars 4
	     :events '((:time (1 1 1) :key 60 :dur q)
		       (:time (2 1 1) :key 61 :dur q)
		       (:time (3 1 1) :key 62 :dur q)
		       (:time (4 1 1) :key 63 :dur q)))

(transformer tx3 sa3
	     :transform #'(lambda (part event)
			    (declare (ignore part))
			    (let* ((time (car event))
				   (msg (cdr event))
				   (key (data msg 0))
				   (vel (data msg 1))
				   (new-channel 6))
			      (list (cons time (if (midi-note-on-p msg)
						   (midi-note-on (1- new-channel) key vel)
						 (midi-note-off (1- new-channel) key 64)))))))

(let* ((events (render-once tx3)))
  (pass? "channel-shift, transform test 3"
	 (every #'(lambda (event)
		    (= 5 (channel-index (cdr event))))
		events)))
