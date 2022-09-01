;;;; CYCO composition time-signature.lisp
;;;;
;;;; Defines time-signature class
;;;;

(in-package :cyco)

(defclass time-signature (cyco-node) ()
  (:documentation
   "TIME-SIGNATURE is a type of node which defines a nested timing 
structure.  The CYCO notion of a time signature is a generalization 
of the typical view.  In addition to the standard 'beats-per-measure' 
and basic beat unit,  time-signature defines tempo in BPM, 
number of bars in a phrase and the number of sub-beats to a beat.
The properties tbeats and tsubbeats are tripplet version of beats and 
subbeats"))

(defgeneric time-signature-p (obj))
(defmethod time-signature-p ((tsig time-signature)) t)
(defmethod time-signature-p ((obj t)) nil)

(constant +time-signature-properties+
	  '(:tempo :unit :bars :beats :tbeats :subbeats :tsubbeats
		   :phrase-duration :bar-duration :beat-duration :tbeat-duration
		   :subbeat-duration :tsubbeat-duration :tick-duration
		   :ticks-per-beat))

(defmethod init-time-signature ((node cyco-node))
  (dolist (c (children node))(init-time-signature c)))
  
(defmethod init-time-signature ((time-signature time-signature))
  (let* ((tick-duration (/ (* 60.0 (metric (property time-signature :unit)))
			   (* (property time-signature :tempo)
			      *ticks-per-beat*)))
	 (beat-duration (* tick-duration *ticks-per-beat*))
	 (bar-duration (* beat-duration (property time-signature :beats)))
	 (phrase-duration (* bar-duration (property time-signature :bars)))
	 (subbeat-duration (/ beat-duration (property time-signature :subbeats))))
    (put time-signature :tick-duration tick-duration)
    (put time-signature :subbeat-duration subbeat-duration)
    (put time-signature :beat-duration beat-duration)
    (put time-signature :bar-duration bar-duration)
    (put time-signature :phrase-duration phrase-duration)
    (put time-signature :tbeats (truncate (* 3/2 (property time-signature :beats))))
    (put time-signature :tbeat-duration (float (* 2/3 beat-duration)))
    (put time-signature :tsubbeats (* 3/2 (property time-signature :subbeats)))
    (put time-signature :tsubbeat-duration (float (* 2/3 subbeat-duration)))
    (put time-signature :ticks-per-beat *ticks-per-beat*)
    (dolist (c (children time-signature))(init-time-signature c))
    time-signature))

;; *root-time-signature* provides defaults for new instances of time-signature.
;;
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

(defun set-default-time-signature (&key tempo unit bars beats subbeats ticks)
  (if tempo (put *root-time-signature* :tempo tempo))
  (if unit (put *root-time-signature* :unit unit))
  (if bars (put *root-time-signature* :bars bars))
  (if beats (put *root-time-signature* :beats beats))
  (if subbeats (put *root-time-signature* :subbeats subbeats))
  (if ticks
      (progn
	(setf *ticks-per-beat* ticks)
	(put *root-time-signature* :ticks-per-beat ticks)))
  (init-time-signature *root-time-signature*)
  *root-time-signature*)


(let ((instance-counter 0))
  (defun time-signature (&key name parent tempo unit bars beats subbeats)
    (let ((new-time-signature (make-instance 'time-signature
					     :properties +time-signature-properties+)))
      (set-name new-time-signature  (->symbol (or name
					       (prog1
						   (sformat "time-signature-~D" instance-counter)
						 (setf instance-counter (1+ instance-counter))))))
      (put new-time-signature :tempo tempo)
      (put new-time-signature :unit unit)
      (put new-time-signature :bars bars)
      (put new-time-signature :beats beats)
      (put new-time-signature :subbeats subbeats)
      (connect (or parent *root-time-signature*) new-time-signature)
      (init-time-signature new-time-signature)
      new-time-signature)))


(defmethod tempo ((time-signature time-signature))
  (property time-signature :tempo))

(defmethod unit ((time-signature time-signature))
  (property time-signature :unit))

(defmethod bars ((time-signature time-signature))
  (property time-signature :bars))

(defmethod beats ((time-signature time-signature))
  (property time-signature :beats))

(defmethod tbeats ((time-signature time-signature))
  (property time-signature :tbeats))

(defmethod subbeats ((time-signature time-signature))
  (property time-signature :subbeats))

(defmethod tsubbeats ((time-signature time-signature))
  (property time-signature :tsubbeats))

(defmethod phrase-duration ((time-signature time-signature))
  (property time-signature :phrase-duration)) 

(defmethod bar-duration ((time-signature time-signature))
  (property time-signature :bar-duration)) 

(defmethod beat-duration ((time-signature time-signature))
  (property time-signature :beat-duration))

(defmethod tbeat-duration ((time-signature time-signature))
  (property time-signature :tbeat-duration)) 

(defmethod subbeat-duration ((time-signature time-signature))
  (property time-signature :subbeat-duration))

(defmethod tsubbeat-duration ((time-signature time-signature))
  (property time-signature :tsubbeat-duration)) 

(defmethod tick-duration ((time-signature time-signature) &key unit)
  (declare (ignore unit))
  (property time-signature :tick-duration))

(defmethod ticks-per-beat ((time-signature time-signature))
  (property time-signature :ticks-per-beat))

(defmethod ticks-per-subbeat ((time-signature time-signature))
  (truncate (/ (ticks-per-beat time-signature)
	       (subbeats time-signature))))

(defmethod set-tempo ((time-signature time-signature)(value number))
  (put time-signature :tempo value)
  (init-time-signature time-signature))

(defmethod set-unit ((time-signature time-signature)(value symbol))
  (put time-signature :unit value)
  (init-time-signature time-signature))

(defmethod set-bars ((time-signature time-signature)(value number))
  (put time-signature :bars value)
  (init-time-signature time-signature))

(defmethod set-beats ((time-signature time-signature)(value number))
  (put time-signature :beats value)
  (init-time-signature time-signature))

(defmethod set-subbeats ((time-signature time-signature)(value number))
  (put time-signature :subbeats value)
  (init-time-signature time-signature))

(defmethod clone ((mother time-signature) &key new-name new-parent)
  (let ((daughter (time-signature)))
    (set-name daughter (if new-name
			(->symbol (sformat new-name (name mother)))))
    (dolist (p (local-properties mother))
      (put daughter (car p)(cdr p)))
    (if new-parent
	(connect new-parent daughter))
    (dolist (c (children mother))
      (let ((c2 (clone c :name new-name)))
	(connect daughter c2)))
    (init-time-signature daughter)
    daughter))
	 
(defmethod ->string ((time-signature time-signature))
  (let ((frmt "~A: ~A ~A BPM ~A bars ~A/~A subbeats=~A"))
    (sformat frmt
	     (type-of time-signature)
	     (name time-signature)
	     (tempo time-signature)
	     (bars time-signature)
	     (beats time-signature)
	     (unit time-signature)
	     (subbeats time-signature))))
	
(defmethod duration ((time-signature time-signature))
  (phrase-duration time-signature))

(defmethod copy-time-signature ((source time-signature)
				(destination time-signature))
  (dolist (key +time-signature-properties+)
    (put destination key (property source key)))
  destination)

(defmethod cue-gamut ((tsig time-signature) &optional (use-tsub nil))
    "Returns list of all time-signature cue-points for BAR 
If optional use-tsub is true, use tsubbeats instead of subbeats."
    (let ((acc '()))
      (if  use-tsub
	  (dolist (br (range 1 (1+ (bars tsig))))
	    (dolist (bt (range 1 (1+ (beats tsig))))
	      (dolist (tsb (range 1 (1+ (tsubbeats tsig))))
		(push (list br bt (->symbol (Sformat "t~D" tsb))) acc))))
	(dolist (br (range 1 (1+ (bars tsig))))
	  (dolist (bt (range 1 (1+ (beats tsig))))
	    (dolist (sb (range 1 (1+ (subbeats tsig))))
	      (push (list br bt sb) acc)))))
      (reverse acc)))

(defmethod cue-points ((tsig time-signature) &optional (use-tsub nil))
  "Returns number of cue-points for standard BAR cue function.
In short returns length of (cue-gamut time-signature)"
  (* (bars tsig)
     (beats tsig)
     (if use-tsub (tsubbeats tsig)(subbeats tsig))))

(defun select-time-signature (obj)
  "Returns time-signature.
Argument may be any of the following:

1) An instance of TIME-SIGNATURE, the argument is returned.

2) A list of form (bars beats subbeats)
   A time-signature with the given parameters is constructed.

3) nil.  If the argument is nil a default time-signature is taken 
   from one of the following sources:
   A) The current section of *project*
   B) If there is no current-section, the value of *project* is used.
   C) If *project* is nil a default 2-bar signature in 4/4 time is 
      returned."

  (cond ((time-signature-p obj)
	 obj)
	((and obj (listp obj))
	 (let ((br (or (car obj) 2))
	       (bt (or (second obj) 4))
	       (sb (or (third obj) 4)))
	   (time-signature :bars br :beats bt :subbeats sb :tempo 60)))
	((and *project* (property *project* :current-section)))
	(*project* *project*)
	(t (time-signature :bars 2 :beats 4 :subbeats 4 :tempo 60))))
	
