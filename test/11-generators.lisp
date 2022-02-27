;;;; test 11-generators
;;;;

(let* ((val 3)
       (gen (constant-value val)))
  (pass? "constant-value 11.1"
	 (every #'(lambda (n)(= n val))(next gen 10))))
       
;; COUNTER
;;

(let* ((gen (counter 0 4))
       (expect '(0 1 2 3 4))
       (actual (next gen 5)))
  (pass? "counter 11.2"
	 (equal expect actual)))

(let* ((gen (counter 0 4 :hook #'(lambda (n)(* n n))))
       (expect '(0 1 4 9 16))
       (actual (next gen 5)))
  (pass? "counter with hook 11.3"
	 (equal expect actual)))

;; RAMP
;;

(let* ((ramp1 (ramp 0 10))
       (expect '(0 1 2 3 4 5 6 7 8 9 10 10 10))
       (actual (next ramp1 (length expect))))
  (pass? "Ramp 11.7"
	 (equal expect actual)))

(let* ((ramp2 (ramp 0 10 :by 2))
       (expect '(0 2 4 6 8 10 10 10))
       (actual (next ramp2 (length expect))))
  (pass? "Ramp 11.8"
	 (equal expect actual)))

(let* ((ramp3 (ramp 10 0 :by 2))
       (expect '(10 8 6 4 2 0 0))
       (actual (next ramp3 (length expect))))
  (pass? "Ramp auto increment sign correction 11.9"
	 (equal expect actual)))

(let* ((ramp4 (ramp 0 10 :by 3))
       (actual (next ramp4 6)))
  (pass? "Ramp end-points with non-uniform increments 11.10"
	 (and (= (car actual) 0)
	      (= (final actual) 10))))

(let* ((ramp5 (ramp 10 0 :by -3))
       (actual (next ramp5 6)))
  (pass? "Ramp end-points with non-uniform negative increments 11.11"
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
  (pass? "asr test 11.12"
	 (and (equal expect-attack actual-attack)
	      (equal expect-sustain actual-sustain)
	      (equal expect-decay actual-decay))))

(let* ((asr2 (asr-envelope 0 10 :attack 2 :decay 3 :sustain 2 :loop nil))
       (expect-attack '(0 2 4 6 8))
       (expect-sustain '(10 10))
       (expect-decay '(7 4 1 0))
       (actual-attack (next asr2 (length expect-attack)))
       (actual-sustain (next asr2 (length expect-sustain)))
       (actual-decay (next asr2 (length expect-decay))))
  (pass? "asr test 11.13"
	 (and
	  (equal expect-attack actual-attack)
	  (equal expect-sustain actual-sustain)
	  (equal expect-decay actual-decay)
	  (every #'zerop (next asr2 10)))))



(let* ((asr3 (asr-envelope 0 5 :attack 1 :decay 1 :sustain 1 :loop t))
       (expect '(0 1 2 3 4  5 5  4 3 2 1 0))
       (actual-1 (next asr3 (length expect)))
       (actual-2 (next asr3 (1- (length expect))))
       (actual-3 (next asr3 (1- (length expect)))))
  (pass? "asr loop mode enabled 11.14"
	 (and (equal expect actual-1)
	      (equal (cdr expect) actual-2)
	      (equal (cdr expect) actual-3))))

	
;; Waveshapes
;;

(flet ((diff (data)
	     (loop for i from 0 to (- (length data) 2)
		   collect (- (nth (1+ i) data)(nth i data)))))
  (let* ((steps 10)
	 (saw (sawtooth 0 9 :cycles 1 :steps steps)))
    (pass? "sawtooth test 11.16"
	   (and (= (length saw) steps)
		(monotonic-p saw)
		(= (mean (diff saw)) 1.0))))

  (let* ((steps 30)
	 (saw (sawtooth 0 9 :cycles 1 :steps steps)))
    (pass? "sawtooth test 11.17"
	   (and (= (length saw) steps)
		(monotonic-p saw)
		(every #'(lambda (n)(<= n 1))(diff saw)))))

  (let* ((steps 30)
	 (saw (sawtooth 0 9 :cycles 2 :steps steps))
	 (first (subseq saw 0 (/ steps 2)))
	 (second (subseq saw (/ steps 2))))
    (pass? "sawtooth cycles 11.18"
	   (and (= (length saw) steps)
		(monotonic-p first)
		(every #'(lambda (n)(<= n 1))(diff saw))
		(equal first second))))

  (let* ((steps 30)
	 (saw (sawtooth 0 9 :cycles 1 :steps steps :phase 180))
	 (first (subseq saw 0 (/ steps 2)))
	 (second (subseq saw (/ steps 2))))
    (pass? "sawtooth phase 11.19"
	   (and (monotonic-p first)
		(= (final first) 9)
		(monotonic-p second)
		(zerop (car second)))))

  (let* ((steps 30)
	 (half (1+ (/ steps 2)))
	 (tri (triangle 0 9 :cycles 1 :steps steps))
	 (first (subseq tri 0 half))
	 (second (subseq tri half)))
    (pass? "triangle 11.20"
	   (and (monotonic-p first)
		(monotonic-p (reverse second)))))
  (let* ((steps 30)
	 (wave (pulse 0 9 :steps steps))
	 (low 0)
	 (high 0))
    (dolist (v wave)
      (if (zerop v)
	  (setf low (1+ low))
	(setf high (1+ high))))
    (pass? "pulse test 11.21"
	   (and (every #'(lambda (a)(member a '(0 9))) wave)
		(= low high)
		(every #'(lambda (n)(= n 9)) (subseq wave 0 (/ steps 3)))
		(every #'zerop (subseq wave (truncate (* 2/3 steps)))))))
  (let* ((steps 31)
	 (wave (pulse 0 9 :steps steps :width 20))
	 (low 0)
	 (high 0))
    (dolist (v wave)
      (if (zerop v)
	  (setf low (1+ low))
	(setf high (1+ high))))
    (pass? "pulse test 11.22"
	   (and (every #'(lambda (a)(member a '(0 9))) wave)
		(= (truncate (/ (float low) high)) 4)))))
 

;; Shift Register
;;

(let* ((sr (shift-register #b0001 #b1000 :mask #b1111))
       (expect '(1 2 4 8))
       (actual (next sr (length expect))))
  (pass? "shift-register 11.23"
	 (equal expect actual)))

(let* ((sr (shift-register #b0001 #b1010 :mask #b1111))
       (expect '(1 2 5 10 4 8))
       (actual (next sr (length expect))))
  (pass? "shift-register 11.24"
	 (equal expect actual)))


;; Hailstone
;;

(let* ((hs (hailstone 10))
       (expect '(10 5 16 8 4 2 1 4))
       (actual (next hs (length expect))))
  (pass? "Hailstone 11.25"
	 (equal expect actual)))

;; Recaman
;;

(let* ((rec (recaman 0))
       (expect '(0 1 3 6 2 7 13 20 12 21 11 22 10 23 9))
       (actual (next rec (length expect))))
  (pass? "Recaman 11.26"
	 (equal expect actual)))

;; Logistic
;;

(let* ((gen (logistic :seed 0.5 :mu 3.75))
       (expected '(0.5 0.9375 0.21972656 0.6429255 0.86089617 0.44907734
		       0.9277758 0.2512795 0.70551795 0.77910894))
       (actual (next gen (length expected))))
  (pass? "Logistic 11.27"
	 (equal expected actual)))

;; Alloy
;;

(let* ((expected '(0 11 22 33 44 55 66 77 88 99))
       (count (length expected))
       (a (counter 0 9 :by 1))
       (b (counter 0 90 :by 10))
       (c (alloy a b :function #'+))
       (actual (next c count)))
  (pass? "Alloy 11.28"
	 (equal actual expected)))

