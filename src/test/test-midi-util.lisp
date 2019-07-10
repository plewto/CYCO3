

(let ((cases '((0 (0))
	       (1 (1))
	       (256 (130 0)))))
  (dolist (test-case cases)
    (let* ((value (car test-case))
	   (expected (second test-case))
	   (actual (int->midi-vlv value)))
      (pass? (same-thing-p expected actual) (sformat "(int->midi-vlv ~A)" value))))
  (dolist (test-case cases)
    (let* ((value (second test-case))
	   (expected (car test-case))
	   (actual (midi-vlv->int value)))
      (pass? (= expected actual)(sformat "(midi-vlv->int ~A)" value)))))

(let* ((test-array #(1 1 1 131 72 1 1 1))
       (result (read-midi-vlv test-array 3))
       (expected (cons 456 2)))
  (pass? (and (consp result)
	      (= (car result)(car expected))
	      (= (cdr result)(cdr expected)))
	 "read-midi-vlv"
	 "Fails due to byte-count being 1 less then specified.
Not sure if fixing would cause downstream problems.")
  (format t "Expected ~A   Actual ~A~%~%" expected result))


(let* ((test-array #(0 0 0 0 1 2 3 4 0 0 0 0))
       (expected (cons 180 4))
       (result (read-midi-long test-array 4)))
  (pass? (and (consp result)
	      (= (car expected)(car result))
	      (= (cdr expected)(cdr result)))
	 "read-midi-long"))

(dolist (test-case '((0.0 #(0 64))
		     (-1.0 #(0 0))
		     (+1.0 #(127 127))))
  (let* ((bend (car test-case))
	 (expected (second test-case))
	 (result (bend->midi-data bend)))
    (pass? (same-thing-p expected result) (sformat "(bend->midi-data ~A)" bend))))

(let ((test-array #(0 64 0 0 127 127))
      (expected (list (cons 0.0 2)
		      (cons -1.0 4)
		      (cons +1.0 6))))
  (let ((offset 0))
    (while (< offset (length test-array))
      (let ((result (read-midi-bend test-array offset))
	    (expect (nth (truncate (/ offset 2)) expected)))
	(pass? (and (consp result)
		    (= (car result)(car expect))
		    (= (cdr result)(cdr expect)))
	       (sformat "read-midi-bend  offset = ~A" offset))
	(setf offset (cdr result))))))

(let ((value 0)
      (passed t))
  (while (and passed (< value 128))
    (setf passed (= (norm->midi-data (midi-data->norm value)))
	  value (1+ value)))
  (pass? passed "norm->midi-data ... midi-data->norm")) 

(not-tested "read-midi-data")
(not-tested "signed-norm->midi-data")
(not-tested "read-signed-midi-data")

(pass? (and (= (bpm->beat-period 60) 1.0)
	    (= (bpm->beat-period 120) 0.5))
       "bpm->beat-duration")

(pass? (and (= (bpm->microseconds 60) 1000000)
	    (= (bpm->microseconds 120) 500000))
       "bpm->microseconds")

(not-tested "tick-duration")
