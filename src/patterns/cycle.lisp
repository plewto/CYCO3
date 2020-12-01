;;;; CYCO Cycle Pattern
;;;;

(defclass cycle (pattern) nil
  (:documentation
   "A CYCLE is a PATTERN where elements are drawn in a 
cyclical manner.  Once all elements have been extracted the
cycle repeats."))

(defmethod cycle-p ((object cycle)) object)

(defun cycle (&key (of '()))
  "Cre4ates new instance of cycle pattern."
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

	 (saw-curve (amp1 amp2 steps phase)
		    (let* ((delta (float (- amp2 amp1)))
			   (increment (/ delta steps)))
		      (rotate (range amp1 amp2 :by increment) phase)))
	 
	 (tri-curve (amp1 amp2 steps phase)
		    (let* ((delta (float (- amp2 amp1)))
			   (increment (* 2 (/ delta steps)))
			   (curve-1 (range amp1 amp2 :by increment))
			   (curve-2 (range amp2 amp1 :by increment)))
		      (rotate (append curve-1 curve-2) phase)))
	 
	 (compare (value threshold high low)
		  (if (< value threshold) low high)) )

  (defun sawtooth (amp1 amp2 &key (steps 16)(phase 0))
    "Creates numeric pattern with sawtooth contour.
amp1 - initial amplitude.
amp2 - final amplitude.
:steps - number of steps, default 16.
:phase - phase shift in degrees, default 0.
Returns Pattern."
    (cycle :of (saw-curve amp1 amp2 steps phase)))
  
  (defun triangle (amp1 amp2 &key (steps 16)(phase 0))
    "Creates numeric pattern with triangle contour.
amp1 - initial amplitude.
amp2 - alternate amplitude
:steps - number of steps, default 16.
:phase - phase shift in degrees, default 0.
Returns Pattern."
      (cycle :of (tri-curve amp1 amp2 steps phase)))


  (defun pulse (amp1 amp2 &key (steps 16)(phase 0)(width 0.5))
    "Creates numeric pulse pattern.
amp1 - initial amplitude
amp2 - alternate amplitude
:steps - number of steps, default 16
:phase - phase shift in degrees, default 0.
:width - pulse width, 0 < width < 1, default 0.5
Returns pulse Pattern."
    (let* ((saw (saw-curve 0.0 1.0 steps phase))
	   (pulse (mapcar #'(lambda (v)(compare v width amp2 amp1)) saw)))
      (cycle :of pulse))))
