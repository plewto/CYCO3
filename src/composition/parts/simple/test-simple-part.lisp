;;;; test-simple-part

(defmethod state-eq ((s1 simple-state)(s2 simple-state))
  (and
   (string= (->string (simple-state-source s1))
   	    (->string (simple-state-source s2)))
   (equal (simple-state-time-specification s1)
	  (simple-state-time-specification s2))
   (eq (simple-state-time s1)
       (simple-state-time s2))
   (equal (simple-state-chord-type s1)
	  (simple-state-chord-type s2))
   (eq (simple-state-chord-inversion s1)
       (simple-state-chord-inversion s2))
   (eq (simple-state-chord-octave s1)
       (simple-state-chord-octave s2))
   (eq (simple-state-articulation s1)
       (simple-state-articulation s2))
   
   (eq (simple-state-dynamic s1)
       (simple-state-dynamic s2))
   (eq (simple-state-key s1)
       (simple-state-key s2))
   (eq (simple-state-pressure s1)
       (simple-state-pressure s2))
   (eq (simple-state-controller-number s1)
       (simple-state-controller-number s2))
   (eq (simple-state-controller-value s1)
       (simple-state-controller-value s2))
   (eq (simple-state-bend s1)
       (simple-state-bend s2))
   (eq (simple-state-program-number s1)
       (simple-state-program-number s2))
   (eq (simple-state-program-bank s1)
       (simple-state-program-bank s2))))

(param reference-state (make-simple-state
			:source ""
			:time-specification '(1 1 1)
			:time 0.0
			:chord-type '(0)
			:chord-inversion 0
			:chord-octave 0
			:articulation 0.0
			:dynamic 0.5
			:key nil
			:pressure nil
			:controller-number nil
			:controller-value nil
			:bend nil
			:program-number nil
			:program-bank nil))

(param test-state (clone reference-state))

(format t "TEST simple-state clone: ~A~%"
	(if (state-eq reference-state test-state)
	    "OK"
	  (error "TEST FAIL: simple-state clone")))

(setf (simple-state-key test-state) 60)

(format t "TEST simple-state key eq: ~A~%"
	(if (eq (simple-state-key test-state) 60)
	    "OK"
	  (error "TEST FAIL: simple-state key eq")))

(transpose test-state 12)

(format t "TEST simple-state transpose: ~A~%"
	(if (eq (simple-state-key test-state) 72)
	    "OK"
	  (error "TEST FAIL: simple-state transpose")))

(invert test-state 67)
(format t "TEST simple-state invert: ~A~%"
	(if (eq (simple-state-key test-state) 62)
	    "OK"
	  (error "TEST FAIL: simple-state invert")))


(setf (simple-state-key test-state) 60
      (simple-state-pressure test-state) 0.5
      (simple-state-controller-number test-state) 0
      (simple-state-controller-value test-state) 0.0
      (simple-state-bend test-state) 0.0
      (simple-state-program-number test-state) 0
      (simple-state-program-bank test-state) 0
      (simple-state-source test-state) "this is a test"
      (simple-state-time-specification test-state) '(2 1 1)
      (simple-state-time test-state) 2.0
      (simple-state-chord-type test-state) '[maj]
      (simple-state-chord-inversion test-state) 2
      (simple-state-chord-octave test-state) 1
      (simple-state-articulation test-state) 1.0
      (simple-state-dynamic test-state) 0.5)
      
(soft-reset test-state)

(format t "TEST simple-state soft-reset: ~A~%"
	(if (state-eq test-state
		      (make-simple-state
		       :source "this is a test"
		       :time-specification '(2 1 1)
		       :time 2.0
		       :chord-type '[maj]
		       :chord-inversion 2
		       :chord-octave 1
		       :articulation 1.0
		       :dynamic 0.5
		       :key nil
		       :pressure nil
		       :controller-number nil
		       :controller-value nil
		       :bend nil
		       :program-number nil
		       :program-bank nil))
	    "OK"
	  (error "TEST FAIL: simple-state soft-reset")))

(reset test-state)
(format t "TEST simple-state reset: ~A~%"
	(if (state-eq test-state
		      (make-simple-state
		       :source ""
		       :time-specification nil
		       :time nil
		       :chord-type '(0)
		       :chord-inversion 0
		       :chord-octave 0
		       :articulation 0.0
		       :dynamic 0.5
		       :key nil
		       :pressure nil
		       :controller-number nil
		       :controller-value nil
		       :bend nil
		       :program-number nil
		       :program-bank nil))
	    "OK"
	  (error "TEST FAIL: simple-state reset")))

      

      
