;;;; CYCO3 src/composition/time-signature
;;;;

(defclass time-signature (cyco-node) ()
  (:documentation
   "TIME-SIGNATURE is a type of node which defines a nested timing 
structure.  The CYCO notion of a time signature is a generalization 
of the typical view.  In addition to the standard 'beats-per-measure' 
and basic beat unit,  time-signature defines tempo in BPM, 
number of bars in a phrase and the number of sub-beats to a beat.
The properties tbeats and tsubbeats are tripplet version of beats and 
subbeats"))

(constant +time-signature-properties+
	  '(:tempo :unit :bars :beats :tbeats :subbeats :tsubbeats
		   :phrase-duration :bar-duration :beat-duration :tbeat-duration
		   :subbeat-duration :tsubbeat-duration :tick-duration))

(defmethod init-time-signature ((node cyco-node))
  (dolist (c (children node))(init-time-signature c)))
  
(defmethod init-time-signature ((tsig time-signature))
  (let* ((tick-duration (/ (* 60.0 (metric (property tsig :unit)))
			   (* (property tsig :tempo)
			      +ticks-per-beat+)))
	 (beat-duration (* tick-duration +ticks-per-beat+))
	 (bar-duration (* beat-duration (property tsig :beats)))
	 (phrase-duration (* bar-duration (property tsig :bars)))
	 (subbeat-duration (/ beat-duration (property tsig :subbeats))))
    (put tsig :tick-duration tick-duration)
    (put tsig :subbeat-duration subbeat-duration)
    (put tsig :beat-duration beat-duration)
    (put tsig :bar-duration bar-duration)
    (put tsig :phrase-duration phrase-duration)
    (put tsig :tbeats (truncate (* 3/2 (property tsig :beats))))
    (put tsig :tbeat-duration (float (* 2/3 beat-duration)))
    (put tsig :tsubbeats (* 3/2 (property tsig :subbeats)))
    (put tsig :tsubbeat-duration (float (* 2/3 subbeat-duration)))
    (dolist (c (children tsig))(init-time-signature c))
    tsig))


(global *root-time-signature* (make-instance 'time-signature
				      :name :root-time-signature
				      :transient nil
				      :properties +time-signature-properties+))
(put *root-time-signature* :tempo 60.0)
(put *root-time-signature* :unit 'q)
(put *root-time-signature* :bars 1)
(put *root-time-signature* :beats 4)
(put *root-time-signature* :subbeats 4)
(init-time-signature *root-time-signature*)
;; (global *current-time-signature* *root-time-signature*)  ;; DEPRECIATED


(let ((instance-counter 0))
  (defun time-signature (&key name parent
  		       tempo unit bars beats subbeats)
    (let* ((nm (->symbol (or name
  			     (prog1
  				 (sformat "time-signature-~D" instance-counter)
  			       (setf instance-counter (1+ instance-counter))))))
  	   (tsig (make-instance 'time-signature
  				:name nm
  				:properties +time-signature-properties+)))
      (put tsig :tempo tempo)
      (put tsig :unit unit)
      (put tsig :bars bars)
      (put tsig :beats beats)
      (put tsig :subbeats subbeats)
      (connect (or parent *root-time-signature*) tsig)
      (init-time-signature tsig)
      tsig)))


(defmethod tempo ((tsig time-signature))
  (property tsig :tempo))

(defmethod unit ((tsig time-signature))
  (property tsig :unit))

(defmethod bars ((tsig time-signature))
  (property tsig :bars))

(defmethod beats ((tsig time-signature))
  (property tsig :beats))

(defmethod tbeats ((tsig time-signature))
  (property tsig :tbeats))

(defmethod subbeats ((tsig time-signature))
  (property tsig :subbeats))

(defmethod tsubbeats ((tsig time-signature))
  (property tsig :tsubbeats))

(defmethod phrase-duration ((tsig time-signature))
  (property tsig :phrase-duration)) 

(defmethod bar-duration ((tsig time-signature))
  (property tsig :bar-duration)) 

(defmethod beat-duration ((tsig time-signature))
  (property tsig :beat-duration))

(defmethod tbeat-duration ((tsig time-signature))
  (property tsig :tbeat-duration)) 

(defmethod subbeat-duration ((tsig time-signature))
  (property tsig :subbeat-duration))

(defmethod tsubbeat-duration ((tsig time-signature))
  (property tsig :tsubbeat-duration)) 

(defmethod tick-duration ((tsig time-signature) &key unit)
  (dismiss unit)
  (property tsig :tick-duration))

(defmethod ticks-per-beat ((tsig time-signature))
  +ticks-per-beat+)

(defmethod ticks-per-subbeat ((tsig time-signature))
  (truncate (/ (ticks-per-beat tsig)
	       (subbeats tsig))))

(defmethod tempo! ((tsig time-signature)(value number))
  (put tsig :tempo value)
  (init-time-signature tsig))

(defmethod unit! ((tsig time-signature)(value symbol))
  (put tsig :unit value)
  (init-time-signature tsig))

(defmethod bars! ((tsig time-signature)(value number))
  (put tsig :bars value)
  (init-time-signature tsig))

(defmethod beats! ((tsig time-signature)(value number))
  (put tsig :beats value)
  (init-time-signature tsig))

(defmethod subbeats! ((tsig time-signature)(value number))
  (put tsig :subbeats value)
  (init-time-signature tsig))

(defmethod clone ((src time-signature) &key new-name new-parent)
  (let* ((n (if new-name
		(->symbol (sformat new-name (name src)))))
	 (other (time-signature :name n)))
    (dolist (p (local-properties src))
      (put other (car p)(cdr p)))
    (if new-parent
	(connect new-parent other))
    (dolist (c (children src))
      (let ((c2 (clone c :name new-name)))
	(connect other c2)))
    (init-time-signature other)
    other))
	 
(defmethod ->string ((tsig time-signature))
  (let ((frmt "~A: ~A ~A BPM ~A bars ~A/~A subbeat=~A"))
    (sformat frmt
	     (type-of tsig)
	     (name tsig)
	     (tempo tsig)
	     (bars tsig)
	     (beats tsig)
	     (unit tsig)
	     (subbeats tsig))))
	
(defmethod duration ((tsig time-signature))
  (phrase-duration tsig))

(defmethod copy-time-signature ((src time-signature)
				(dst time-signature))
  (dolist (key +time-signature-properties+)
    (put dst key (property src key)))
  dst)

