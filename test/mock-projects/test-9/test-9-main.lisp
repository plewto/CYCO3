;;;; CYCO mock project test-9, part time-shift
;;;;

(version 3)
(project test-9 :bars 16
	 :tempo 120
	 :project-directory (join-path *mock-project-directory* "test-9"))

(plugin general-midi)
(prune-orchestra)
(general-midi-instrument piano :channel 1)

(section alpha)

;; Absolute shift in seconds
;;

(param *test-9-cue-list* '((8 1 1)))

(labels ((extract-times (part)
			(mapcar #'car (render-once part)))

	 (shift-values (n time-list)
		       (if (not time-list)
			   nil
			 (cons (+ n (car time-list))
			       (shift-values n (cdr time-list)))))
	 
	 (make-test-qball (name shift)
			  (make-qball name piano
				      :shift shift
				      :cue *test-9-cue-list*
				      :key 60
				      :amp 'mf
				      :dur 'w)) )
  (let* ((qb-reff (make-test-qball 'qb-reff 0))
	 (reff-times (extract-times qb-reff)))

    ;; Absolute positive time shifts
    (loop for shift from 0 to (car reff-times)
	  do (let* ((expected-times (shift-values shift reff-times))
		    (qb-test (make-test-qball 'test shift))
		    (actual-times (extract-times qb-test)))
	       (pass? (sformat "QBall time shift ~A, test 09.01" shift)
		      (equal expected-times actual-times))))
    
    ;; Absolute negative time shifts
    (loop for shift from 0 to (car reff-times)
	  do (let* ((nshift (* -1 shift))
		    (expected-times (shift-values nshift reff-times))
		    (qb-test (make-test-qball 'test nshift))
		    (actual-times (extract-times qb-test)))
	       (pass? (sformat "QBall time shift ~A, test 09.02" nshift)
		      (equal expected-times actual-times))))

    (let ((actual (extract-times (make-test-qball 'test 'w)))
	  (expected (shift-values (bar-duration qb-reff) reff-times)))
      (pass? "QBall symbolic time-shift, test 09.03"
	     (equal actual expected))) ))
      


    

	 
  
    
