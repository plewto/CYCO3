;;;; CYCO parts hold.lisp
;;;;
;;;; Note filter for strummer.

(in-package :cyco-part)

(defun hold-repeated-notes (events)
  (let ((open-notes (->vector (copies 128 0)))
	(acc '()))
    (dolist (event events)
      (let ((message (cdr event)))
	(cond ((midi-note-off-p message)
	       (let* ((keynumber (aref (data-array message) 0))
		      (count (aref open-notes keynumber)))
		 (setf (aref open-notes keynumber) (max (1- count) 0))
		 (if (<= count 1)
		     (push event acc))))

	      ((midi-note-on-p message)
	       (let* ((keynumber (aref (data-array message) 0))
		      (count (aref open-notes keynumber)))
		 (setf (aref open-notes keynumber)(1+ count))
		 (if (zerop count)
		     (push event acc))))
	 
	      (t (push event acc)))))
    (sort-midi-events acc)))

		 
(setf (documentation 'hold-repeated-notes 'function)
      "Filters out overlapping note on/off events with the 
same keynumber.

HOLD-REPEATED-NOTES is a helper function used when the 
STRUMMER :hold argument is true. It is only valid when 
all note events are on the same MIDI channel.")
