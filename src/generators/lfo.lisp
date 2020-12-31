;;;; CYCO generators/lfo.lisp
;;;;

(labels ((rotate (curve phase)
		 (if (zerop (rem phase 360))
		     curve
		   (let* ((index (truncate (/ (* (length curve)(rem phase 360)) 360)))
			  (head (nthcdr index curve))
			  (tail (subseq curve 0 index)))
		     (append head tail))))

	 (compare (value threshold high low)
		  (if (< value threshold) low high)))

  (defun sawtooth-curve (amp1 amp2 &key (cycles 1) steps (phase 0) &allow-other-keys)
    (setf steps (or steps (abs (- amp2 amp1))))
    (let* ((segment-length (truncate (/ steps cycles)))
	   (segment (irange amp1 amp2 segment-length))
	   (curve (reverse (flatten (copies cycles segment)))))
      (while (< (length curve) steps)
	(push amp2 curve))
      (rotate (reverse curve) phase)))


  (defun triangle-curve (amp1 amp2 &key (cycles 1) steps (phase 0) &allow-other-keys)
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


  (defun pulse-curve (amp1 amp2 &key (cycles 1) steps (phase 0)(width 50) &allow-other-keys)
    (let* ((saw (sawtooth-curve 0 100 :cycles cycles :steps steps :phase phase)))
      (mapcar #'(lambda (v)(compare v width amp1 amp2)) saw))) )

    
(defclass lfo (generator)
  ((curve
    :type list
    :accessor lfo-curve
    :initform (triangle-curve 0 16)
    :initarg :curve)
   (pointer
    :type integer
    :accessor lfo-pointer
    :initform 0)))

(defmethod reset ((lfo lfo))
  (setf (lfo-pointer lfo) 0
	(current-value lfo)(car (lfo-curve lfo)))
  lfo)

(defmethod next-1 ((lfo lfo))
  (prog1
      (value lfo)
    (let* ((curve (lfo-curve lfo))
	   (pointer (rem (1+ (lfo-pointer lfo))(length curve))))
      (setf (current-value lfo)(nth pointer curve)
	    (lfo-pointer lfo) pointer))))


(defun lfo (&key (curve (triangle-curve 0 16)) (hook #'(lambda (n) n)) &allow-other-keys)
  (let ((gen (make-instance 'lfo
			    :hook hook
			    :curve curve)))
    (reset gen)))


(defmethod clone ((mother lfo) &key new-name new-parent)
  (declare (ignore new-name new-parent))
  (let ((daughter (lfo :curve (lfo-curve mother)
		       :hook (value-hook mother))))
    (setf (lfo-pointer daughter)(lfo-pointer mother))
    daughter))


(setf (documentation 'sawtooth-curve 'function)
      "Returns numeric list with sawtooth contour.
amp1     - trough amplitude
amp2     - peak amplitude
:cycles  - number of wave cycles, default 1.
:steps   - number of steps, default |amp2 - amp1|
:phase   - phase shift in degrees, default 0

The resulting curve may have slight distortions.   Specifically
1) Increment values may not be consistent.
2) The curve may contain padding values.")

(setf (documentation 'triangle-curve 'function)
      "Returns numeric list with triangle contour.
amp1     - trough amplitude
amp2     - peak amplitude
:cycles  - number of wave cycles, default 1.
:steps   - number of steps, default |amp2 - amp1|
:phase   - phase shift in degrees, default 0

The resulting curve may have slight distortions.   Specifically
1) Increment values may not be consistent.
2) The curve may contain padding values.")


(setf (documentation 'pulse-curve 'function)
      "Returns numeric list with pulse contour.
amp1     - trough amplitude
amp2     - peak amplitude
:cycles  - number of wave cycles, default 1.
:steps   - number of steps, default |amp2 - amp1|
:phase   - phase shift in degrees, default 0
:width   - pulse width percent, 0 < width < 100, default 50.

The resulting curve may have slight distortions.   Specifically
1) Increment values may not be consistent.
2) The curve may contain padding values.")


(setf (documentation 'lfo 'function)
      "Generator which warps numeric list.
:curve - Numeric wave-table, defaults to triangle wave-shape.
:hook  - hook function applied to value, default identity (lambda (n)) --> n

Examples

   (next (lfo) 16) --> (0 2 4 6 8 10 12 14 16 14 11 9 7 5 2 0)
   (next (lfo :curve (pulse-curve 0 1 :steps 8)) 16) --> (1 1 1 1 0 0 0 0 1 1 1 1 0 0 0 0)")





