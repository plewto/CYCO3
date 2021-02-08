;;;; CYCO3 examples ex3 question.lisp
;;;;


(let ((time-signature (property *project* :current-section)))

  (labels ((key-list (&optional (end-note 'c5))
		     (append '(bf4 cs4 e4 ef5) (list end-note)))
	   
	   (duration-list (&optional (end-duration 'ht))
			  (append '(h+qt qt qt h+e)(list end-duration)))
	   
	   (shift-offset (start-time)
			 (bar time-signature start-time))

	   (the-question (name start-time &key (end-note 'c5)(end-duration 'h)(amp 'mf))
			 (make-qball name question
				     :amp amp
				     :shift (shift-offset start-time)
				     :cue '((1 1 1)(1 T5 1)(1 T6 1)(2 1 1)(2 3 3))
				     :key (key-list end-note)
				     :dur (duration-list end-duration)))

	   
	   (envelope (name start-time &key (decay-start '(2 4 2))(decay-stop '(3 1 1)))
		     (let* ((shift (shift-offset start-time))
			    (a0   0)  ;; initial amp
			    (a1 127)  ;; peak amp
			    (a2   0)  ;; final amp
			    (attack (list :time '(1 1 1) '(1 1 2) 't :value a0 a1 :ctrl 'volume :ramp))
			    (decay  (list :time decay-start decay-stop 't :value a1 a2 :ctrl 'volume :ramp)))
		       (make-controllers name question
					 :shift shift
					 :events (list
						  attack
						  decay
						  )))))
    
    (the-question 'q1 '(16  1))
    (the-question 'q2 '(23 T5) :end-note 'b4)
    (the-question 'q3 '(31  2))
    (the-question 'q4 '(38 T5) :end-note 'b4)
    (the-question 'q5 '(45 T3))
    (the-question 'q6 '(50  1) :end-note 'b4)
    (the-question 'q7 '(58 T5) :end-note 'b4)

    (envelope 'eq1 '(16  1))
    (envelope 'eq2 '(23 t5))
    (envelope 'eq3 '(31  2))
    (envelope 'eq4 '(38 T5))
    (envelope 'eq5 '(45 T3))
    (envelope 'eq6 '(51  1))
    (envelope 'eq7 '(58 T5) :decay-start '(2 1 1))))
