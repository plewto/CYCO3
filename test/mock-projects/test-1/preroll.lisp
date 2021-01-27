;;;; project test-1 preroll
;;;;

(preroll
 :bars 2
 :metronome t
 :instruments (list piano bass organ *metronome*))

(pass? "preroll created"
       (and (boundp 'preroll)
	    (section-p preroll)))


(param -preroll-events- (render-once preroll))



;; Check for expected program-change events.
;;    number of events
;;    channels and program numbers.
;;
(let* ((program-events (remove-if-not #'(lambda (event)(midi-program-change-p (cdr event))) -preroll-events-))
       (expect '((0 0)(1 32)(2 16)(15 115))))  ;; (channel-index program-number)
  (pass? "Mock project test-1  preroll section, program-change events"
	 (and (= (length program-events) 4)
	      (every #'(lambda (q)(and (zerop (car q))(midi-program-change-p (cdr q)))) program-events)))

  (pass? "Mock project test-1  preroll section, program-change channel/program-number."
	 (let ((flag t))
	   (dolist (ex expect)
	     (setf flag (and flag (some #'(lambda (event)
					    (let ((message (cdr event)))
					      (and (= (channel-index message)(car ex))
						   (= (data message 0)(second ex)))))
					program-events))))
	   flag)))
 
;; Checks for expected metronome events.
;;    number of not-on events
;;    time and key-number sequence.
;;
(let* ((kmap (keynumber-map *metronome*))
       (phrase (funcall kmap :phrase))
       (bar (funcall kmap :bar))
       (beat (funcall kmap :beat))
       (expect-keys (list phrase beat beat beat bar beat beat beat))
       (expect-times (range 0 8))
       (on-events (remove-if-not #'(lambda (event)(midi-note-on-p (cdr event))) -preroll-events-)))
  (pass? "Mock project test-1  preroll section, metronome event count."
	 (= (length on-events)(length expect-keys)(length expect-times)))
  (loop for event in on-events
	for time in expect-times
	for key in expect-keys
	do (pass? "Mock project test-1 preroll section, metronome events."
		  (and (= (car event) time)
		       (= (data (cdr event) 0) key)))))

