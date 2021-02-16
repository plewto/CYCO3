;;;; CYCO generators/lfo.lisp
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

    
(defclass lfo (generator)
  ((curve
    :type list
    :accessor lfo-curve
    :initform (triangle 0 16)
    :initarg :curve)
   (pointer
    :type integer
    :accessor lfo-pointer
    :initform 0))
  (:documentation
   "LFO is a generator which produces cyclical values."))

(defmethod pattern-length ((lfo lfo) &key &allow-other-keys)
  (length (lfo-curve lfo)))

(defmethod reset ((lfo lfo))
  (setf (lfo-pointer lfo) 0
	(current-value lfo)(car (lfo-curve lfo)))
  lfo)

(defmethod next-1 ((lfo lfo))
  (prog1
      (value lfo)
    (let* ((curve (lfo-curve lfo))
	   (pointer (rem (1+ (lfo-pointer lfo))(length curve)))
	   (v (nth pointer curve)))
      (setf (current-value lfo)
	    (if (funcall (monitor lfo) v)
		(funcall (action lfo) lfo v)
	      v))
      (setf (lfo-pointer lfo) pointer))))


(defun lfo (&key (curve (triangle 0 127))
		 (hook #'(lambda (n) n))
		 (monitor #'(lambda (value)
			      (declare (ignore value))
			      nil))
		 (action #'(lambda (lfo value)
			     (declare (ignore lfo))
			     value))
		 &allow-other-keys)
  (let ((gen (make-instance 'lfo
			    :hook hook
			    :monitor monitor
			    :action action
			    :curve curve)))
    (reset gen)))


(defmethod clone ((mother lfo) &key &allow-other-keys)
  (lfo :curve (lfo-curve mother)
       :monitor (monitor mother)
       :action (action mother)
       :hook (value-hook mother)))


(setf (documentation 'sawtooth 'function)
      "Returns numeric list with sawtooth contour.

(sawtooth amp1 amp2 &key cycles steps phase)

amp1     - trough amplitude
amp2     - peak amplitude
:cycles  - number of wave cycles, default 1.
:steps   - number of steps, default |amp2 - amp1|
:phase   - phase shift in degrees, default 0

The resulting curve may have slight distortions.   Specifically
1) Increment values may not be consistent.
2) The curve may contain padding values.")

(setf (documentation 'triangle 'function)
      "Returns numeric list with triangle contour.

(triangle amp1 amp2 &key cycles steps phase)

amp1     - trough amplitude
amp2     - peak amplitude
:cycles  - number of wave cycles, default 1.
:steps   - number of steps, default |amp2 - amp1|
:phase   - phase shift in degrees, default 0

The resulting curve may have slight distortions.   Specifically
1) Increment values may not be consistent.
2) The curve may contain padding values.")


(setf (documentation 'pulse 'function)
      "Returns numeric list with pulse contour.

(pulse amp1 amp2 &key cycles steps phase width)

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
     "Returns new instance of LFO.

(lfo &key curve hook monitor action)

:curve   - Numeric list describing the output sequence.
           The triangle, sawtooth and pulse functions may be used
           for the corresponding contour.
           Default (triangle 0 127).
:hook    - Function applied by the value method to the internal-value.
           Default (lambda (n) n).
:monitor - Predicate called by next-1 to determine if action function
           should be executed.  Default (lambda (value) nil).
:action  - Function called within next-1 whenever the monitor predicate
           returns non-nil.  Default (lambda (lfo value) value)")
