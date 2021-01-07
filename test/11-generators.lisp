;;;; test 11-generators
;;;;

(let* ((val 3)
       (gen (constant-value val)))
  (pass? "constant-value"
	 (every #'(lambda (n)(= n val))(next gen 10))))
       


;; COUNTER
;;

(let* ((gen (counter))
       (expect '(0 1 2 3 4))
       (actual (next gen 5)))
  (pass? "counter"
	 (equal expect actual)))

(let* ((gen (counter :hook #'(lambda (n)(* n n))))
       (expect '(0 1 4 9 16))
       (actual (next gen 5)))
  (pass? "counter with hook"
	 (equal expect actual)))


(let ((action-counter 0))
  (flet ((action-hook (n)
		      (declare (ignore n))
		      (setf action-counter (1+ action-counter))))
    (let* ((gen1 (countdown 3))
	   (expect '(3 2 1 0 0))
	   (actual (next gen1 5)))
      (pass? "countdown test 1"
	     (equal expect actual)))
    
    (let* ((gen2 (countdown 3 :action #'action-hook :multi-trigger nil))
	   (expect '(3 2 1 0 0))
	   (actual (next gen2 5)))
      (pass? "countdown test 2 with single action"
	     (and (equal expect actual)
		  (= action-counter 1))))

    (setf action-counter 0)
    (let* ((gen3 (countdown 3 :action #'action-hook :multi-trigger t)))
      (next gen3 5)
      (pass? "countdown test 3 with multi action"
      	     (= action-counter 2))) ))

;; RAMP
;;

(let* ((ramp1 (ramp 0 10))
       (expect '(0 1 2 3 4 5 6 7 8 9 10 10 10))
       (actual (next ramp1 (length expect))))
  (pass? "Ramp test 1"
	 (equal expect actual)))

(let* ((ramp2 (ramp 0 10 :by 2))
       (expect '(0 2 4 6 8 10 10 10))
       (actual (next ramp2 (length expect))))
  (pass? "Ramp test 2"
	 (equal expect actual)))

(let* ((ramp3 (ramp 10 0 :by 2))
       (expect '(10 8 6 4 2 0 0))
       (actual (next ramp3 (length expect))))
  (pass? "Ramp test 3, auto increment sign correction"
	 (equal expect actual)))

(let* ((ramp4 (ramp 0 10 :by 3))
       (actual (next ramp4 6)))
  (pass? "Ramp test 4, end-points with non-uniform increments"
	 (and (= (car actual) 0)
	      (= (final actual) 10))))

(let* ((ramp5 (ramp 10 0 :by -3))
       (actual (next ramp5 6)))
  (pass? "Ramp test 4, end-points with non-uniform negative increments"
	 (and (= (car actual) 10)
	      (- (final actual) 0))))

;; ASR ENVELOPE
;;

(let* ((asr1 (asr-envelope 0 10 :sustain 4))
       (expect-attack '(0 1 2 3 4 5 6 7 8 9))
       (expect-sustain '(10 10 10 10))
       (expect-decay (reverse expect-attack))
       (actual-attack (next asr1 (length expect-attack)))
       (actual-sustain (next asr1 (length expect-sustain)))
       (actual-decay (next asr1 (length expect-decay))))
  (pass? "asr test 1"
	 (and (equal expect-attack actual-attack)
	      (equal expect-sustain actual-sustain)
	      (equal expect-decay actual-decay))))

(let* ((asr2 (asr-envelope 0 10 :attack 2 :decay 3 :sustain 2))
       (expect-attack '(0 2 4 6 8))
       (expect-sustain '(10 10))
       (expect-decay '(7 4 1 0))
       (actual-attack (next asr2 (length expect-attack)))
       (actual-sustain (next asr2 (length expect-sustain)))
       (actual-decay (next asr2 (length expect-decay))))
  (pass? "asr test 2"
	 (and (equal expect-attack actual-attack)
	      (equal expect-sustain actual-sustain)
	      (equal expect-decay actual-decay)
	      (every #'zerop (next asr2 10)))))
	      
(let* ((asr3 (asr-envelope 0 5 :attack 1 :decay 1 :sustain 1 :cycle t))
       (expect '(0 1 2 3 4  5 5  4 3 2 1 0))
       (actual-1 (next asr3 (length expect)))
       (actual-2 (next asr3 (1- (length expect))))
       (actual-3 (next asr3 (1- (length expect)))))
  (pass? "asr test 3, cycle mode enabled"
	 (and (equal expect actual-1)
	      (equal (cdr expect) actual-2)
	      (equal (cdr expect) actual-3))))
	
