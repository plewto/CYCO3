;;;; CYCO generators/waveshapes.lisp
;;;;
;;;; Generators with cyclical output.

(in-package :cyco)

(labels ((rotate (curve phase)
		 (if (zerop (rem phase 360))
		     curve
		   (let* ((index (truncate (/ (* (length curve)(rem phase 360)) 360)))
			  (head (nthcdr index curve))
			  (tail (subseq curve 0 index)))
		     (append head tail))))

	 (compare (value threshold high low)
		  (if (< value threshold) low high)))

  (defun sawtooth (amp1 amp2 &key (cycles 1) steps (phase 0) &allow-other-keys)
    (setf steps (or steps (abs (- amp2 amp1))))
    (let* ((segment-length (truncate (/ steps cycles)))
	   (segment (irange amp1 amp2 segment-length))
	   (curve (reverse (flatten (copies cycles segment)))))
      (while (< (length curve) steps)
	(push amp2 curve))
      (rotate (reverse curve) phase)))


  (defun triangle (amp1 amp2 &key (cycles 1) steps (phase 0) &allow-other-keys)
    (setf steps (or steps (abs (- amp2 amp1))))
    (let* ((cycle-steps (/ steps cycles))
	   (half (truncate (/ cycle-steps 2)))
	   (half-2 (if (evenp half) half (1+ half)))
	   (curve-1 (irange amp1 amp2 half))
	   (curve-2 (cdr (irange amp2 amp1 (1+ half-2))))
	   (curve (flatten (copies cycles (append curve-1 curve-2)))))
      (while (< (length curve) steps)
	(push amp1 curve))
      (rotate (reverse curve) phase)))


  (defun pulse (amp1 amp2 &key (cycles 1) steps (phase 0)(width 50) &allow-other-keys)
    (let* ((saw (sawtooth 0 100 :cycles cycles :steps steps :phase phase)))
      (mapcar #'(lambda (v)(compare v width amp1 amp2)) saw))) )

 
