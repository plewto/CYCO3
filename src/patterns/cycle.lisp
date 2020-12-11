;;;; CYCO pattern cycle.lisp
;;;;
;;;; A CYCLE pattern generates cyclical values.
;;;;

(in-package :cyco)

(defclass cycle (pattern) nil
  (:documentation
   "A CYCLE is a PATTERN where elements are drawn in a 
cyclical manner.  Once all elements have been extracted the
cycle repeats."))

(defmethod cycle-p ((object cycle)) object)

(defun cycle (&key (of '()))
  "Creates new instance of cycle pattern."
  (let ((q (make-instance 'cycle :of (->list of))))
    (reset q)
    q))

(defmethod next-1 ((q cycle))
  (let* ((ptr (pointer q))
	 (val (next-1 (nth ptr (elements q)))))
    (setf (pointer q)
	  (rem (1+ ptr)(cardinality q)))
    (setf (slot-value q 'value) val)
    val))

(defmethod ->cycle ((object cycle)) object)

(defmethod ->cycle ((lst list))
  (cycle :of lst))

(defmethod ->cycle ((v vector))
  (cycle :of (->list v)))

(defmethod ->cycle ((object t))
  (cycle :of (->list object)))


(labels ((rotate (curve phase)
		 (if (zerop (rem phase 360))
		     curve
		   (let* ((index (truncate (/ (* (length curve)(rem phase 360)) 360)))
			  (head (nthcdr index curve))
			  (tail (subseq curve 0 index)))
		     (append head tail))))
	 
	 (saw-curve (amp1 amp2 cycles steps phase)
		    (let* ((delta (float (abs (- amp2 amp1))))
			   (increment (* cycles (/ delta (1- steps))))
			   (value (min amp1 amp2))
			   (acc '()))
		      (while (<= value (max amp1 amp2))
			(push value acc)
			(setf value (+ value increment)))
		      (flatten (copies cycles (rotate (reverse acc) phase)))))
		     
	 (tri-curve (amp1 amp2 cycles steps phase)
		    (let* ((delta (float (abs (- amp2 amp1))))
			   (increment (* 2 cycles (/ delta steps)))
			   (curve-1 (range amp1 amp2 :by increment))
			   (curve-2 (range amp2 amp1 :by increment)))
		      (flatten (copies cycles (rotate (append curve-1 curve-2) phase)))))
	 
	 (compare (value threshold high low)
		  (if (< value threshold) low high))


	 (isaw-curve (amp1 amp2 cycles steps phase)
		     (let* ((segment-length (truncate (/ steps cycles)))
			    (segment (irange amp1 amp2 segment-length))
			    (curve (flatten (copies cycles segment))))
		       (setf curve (reverse curve))
		       (while (< (length curve) steps)
			 (push amp2 curve))
		       (rotate (reverse curve) phase)))

	 (itri-curve (amp1 amp2 cycles steps phase)
		     (let* ((cycle-steps (/ steps cycles))
			    (half (truncate (/ cycle-steps 2)))
			    (half-2 (if (evenp half) half (1+ half)))
			    (curve-1 (irange amp1 amp2 half))
			    (curve-2 (cdr (irange amp2 amp1 (1+ half-2))))
			    (curve (flatten (copies cycles (append curve-1 curve-2))))
			    )
		       (while (< (length curve) steps)
			 (push amp1 curve))
		       (rotate (reverse curve) phase)))
	 
	 

	 )

  (defun sawtooth (amp1 amp2 &key (cycles 1)(steps 16)(phase 0) &allow-other-keys)
    "Creates numeric pattern with sawtooth contour.  Note amp2 
value may never be reached.
amp1 - minimum amplitude.
amp2 - peak amplitude.
:cycles - number of sawtooth cycles, default 1 
:steps  - number of steps, default 16.
:phase  - phase shift in degrees, default 0.
Returns Pattern."
    (cycle :of (saw-curve amp1 amp2 cycles steps phase)))


  (defun isawtooth (amp1 amp2 &key (cycles 1)(steps 16)(phase 0) &allow-other-keys)
    "Creates integer pattern with sawtooth contour.  
Unlike the general sawtooth function, the initial and final values of isawtooth 
are guaranteed to be amp1 and amp2 respectively.  However the wave-shape may be distorted.
In particular step sizes may not be uniform and values may repeat.

amp1    - Integer, initial amplitude.
amp2    - Integer, final amplitude.
:cycles - Integer, number of sawtooth cycles.  For best results cycles should be 
          a factor of steps and be less then or equal to steps/2.  Default 1.
:steps  - Integer, length of pattern, default 16.
:phase  - Integer, phase shift in degrees, default 0
Returns pattern."
    (cycle :of (isaw-curve amp1 amp2 cycles steps phase)))

  
  (defun triangle (amp1 amp2 &key (cycles 1)(steps 16)(phase 0) &allow-other-keys)
    "Creates numeric pattern with triangle contour.
amp1 - minimum amplitude.
amp2 - peak amplitude.
:cycles - number of triangle cycles, default 1
:steps  - number of steps, default 16.
:phase  - phase shift in degrees, default 0.
Returns Pattern."
      (cycle :of (tri-curve amp1 amp2 cycles steps phase)))

  
  (defun itriangle (amp1 amp2 &key (cycles 1)(steps 16)(phase 0) &allow-other-keys)
    "Creates integer pattern with triangle contour.
Unlike the general triangle function, the initial and final values of itriangle
are guaranteed to be amp1 and amp2 respectively.  However the wave-shape may be distorted.
In particular steps sizes may not be uniform and some values may be repeated.

amp1    - Integer, initial amplitude.
amp2    - Integer, final amplitude.
:cycles - Integer, number of sawtooth cycles.  For best results cycles should be 
          a factor of steps and be less then or equal to steps/2.  Default 1.
:steps  - Integer, length of pattern, default 16.
:phase  - Integer, phase shift in degrees, default 0
Returns pattern."
    (cycle :of (itri-curve amp1 amp2 cycles steps phase)))


  (defun pulse (amp1 amp2 &key (cycles 1)(steps 16)(phase 0)(width 0.5) &allow-other-keys)
    "Creates numeric pulse pattern.
amp1 - minimum amplitude.
amp2 - maximum amplitude.
:cycles - number of pulse cycles, default 1.
:steps  - number of steps, default 16
:phase  - phase shift in degrees, default 0.
:width  - pulse width, 0 < width < 1, default 0.5
Returns pulse Pattern."
    (let* ((saw (saw-curve 0.0 1.0 cycles steps phase))
	   (pulse (mapcar #'(lambda (v)(compare v width amp2 amp1)) saw)))
      (cycle :of pulse)))

  (defun ipulse (amp1 amp2 &key (cycles 1)(steps 16)(phase 0)(width 0.5) &allow-other-keys)
    "Creates integer pulse pattern.
amp1    - Integer, initial amplitude.
amp2    - Integer, final amplitude.
:cycles - Integer, number of sawtooth cycles.  For best results cycles should be 
          a factor of steps and be less then or equal to steps/2.  Default 1.
:steps  - Integer, length of pattern, default 16.
:phase  - Integer, phase shift in degrees, default 0
:width  - Float, pulse width 0.0 <= width <= 1.0, default 0.5
Returns pulse pattern."

    (let* ((curve (reverse (saw-curve 0.0 1.0 cycles steps 0))))
      (while (< (length curve) steps)
	(push 1.0 curve))
      (setf curve (mapcar #'(lambda (v)(compare v width (truncate amp1)(truncate amp2)))
			  curve))
      (cycle :of (rotate curve phase)))) )
  
