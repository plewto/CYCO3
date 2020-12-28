
(let ((cases '((0 (0))
	       (1 (1))
	       (256 (130 0)))))
  (dolist (test-case cases)
    (let* ((value (car test-case))
	   (expected (second test-case))
	   (actual (int->midi-vlv value)))
      (pass? (sformat "int->midi-vlv ~A" (car test-case))
	     (equal expected actual)))))

(let* ((test-array #(1 1 1 131 72 1 1 1))
       (actual (read-midi-vlv test-array 3))
       (expected (cons 456 2)))
  (pass? "read-midi-vlv"
	 (and (consp actual)
	      (= (car actual)(car expected))
	      (= (cdr actual)(cdr expected)))))


(let* ((test-array #(0 0 0 0 1 2 3 4 0 0 0 0))
       (expected (cons 180 4))
       (actual (read-midi-long test-array 4)))
  (pass? "read-midi-long"
	 (and (consp actual)
	      (= (car expected)(car actual))
	      (= (cdr expected)(cdr actual)))))

(dolist (test-case '(( 0.0 #(0 64))
		     (-1.0 #(0 0))
		     (+1.0 #(127 127))))
  (let* ((bend (car test-case))
	 (expected (second test-case))
	 (actual (bend->midi-data bend)))
    (pass? (sformat "bend->midi-data ~A" bend)
	   (and (vectorp actual)
		(= (length actual) 2)
		(= (aref actual 0)(aref expected 0))
		(= (aref actual 1)(aref expected 1))))))

(let ((test-array #(0 64 0 0 127 127))
      (expected (list (cons 0.0 2)
		      (cons -1.0 4)
		      (cons +1.0 6)))
      (offset 0))
  (while (< offset (length test-array))
    (let ((actual (read-midi-bend test-array offset))
	  (expect (nth (truncate (/ offset 2)) expected)))
      (pass? (sformat "read-midi-bend offset = ~A" offset)
	     (and (consp actual)
		  (= (car actual)(car expect))
		  (= (cdr actual)(cdr expect))))
      (setf offset (cdr actual)))))

(let ((value 0)
      (passed t))
  (if (not *silent-pass*)
      (format t "    Testing norm->midi-data and midi-data->norm~%"))
  (while (and passed (< value 128))
    (setf passed (= (norm->midi-data (midi-data->norm value)))
	  value (1+ value))
    (if (not passed)
	(fail (sformat "norm->midi-data  midi-data->norm  ~A" (1- value)) ""))))
	   

(pass? "bpm->beat-period"
       (and (= (bpm->beat-period 60) 1.0)
	    (= (bpm->beat-period 120) 0.5)))

(pass? "bpm->microseconds"
       (and (= (bpm->microseconds 60) 1e6)
	    (= (bpm->microseconds 120) (/ 1e6 2))))


;; duplicate-channel-message test
;;
(let ((sources (list (midi-note-on 1 60 64)
		     (midi-note-off 2 60 64)
		     (midi-poly-pressure 3 60 64)
		     (midi-channel-pressure 4 64)
		     (midi-control-change 5 64 64)
		     (midi-program-change 6 64)
		     (midi-pitch-bend 7 0 0))))
  (dolist (s sources)
    (let ((dup (duplicate-channel-message s)))
      (pass? (sformat "duplicate-channel-message ~A" s)
	     (and (= (channel-index dup)(channel-index s))
		  (= (data dup 0)(data s 0))
		  (= (data dup 1)(data s 1))
		  (not (eq dup s)))))))
	 



(not-tested 'read-midi-data)
(not-tested 'signed-norm->midi-data)
(not-tested 'read-signed-midi-data)
(not-tested 'tick-duration)
