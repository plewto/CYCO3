;;;; CYCO
;;;;
;;;; Defines symbolic representation of note dynamics.
;;;; Dynamics are normalized between 0 and 1 and map to MIDI velocity 0..127
;;;;
;;;; Symbolic dynamic representation use an extension of standard music
;;;; notation; PPPP PPP PP P MP MF F FF FFF FFFF
;;;; For each standard value two additional values, X- and X+, are defined
;;;; for a finer resolution.
;;;;
;;;;   ... F- < F < F+ < FF- < FF < FF+ ...
;;;;

(global *dynamics*
	  (let ((acc (make-hash-table :size 40)))
	    (setf (gethash 'R acc) -1.0)
	    (setf (gethash 'MIN acc) (float 1/127))
	    (setf (gethash 'PPPP- acc) (float 2/127))
	    (setf (gethash 'PPPP acc) (float 6/127))
	    (setf (gethash 'PPPP+ acc) (float 10/127))
	    (setf (gethash 'PPP- acc) (float 14/127))
	    (setf (gethash 'PPP acc) (float 19/127))
	    (setf (gethash 'PPP+ acc) (float 23/127))
	    (setf (gethash 'PP- acc) (float 27/127))
	    (setf (gethash 'PP acc) (float 32/127))
	    (setf (gethash 'PP+ acc) (float 36/127))
	    (setf (gethash 'P- acc) (float 40/127))
	    (setf (gethash 'P acc) (float 45/127))
	    (setf (gethash 'P+ acc) (float 49/127))
	    (setf (gethash 'MP- acc) (float 53/127))
	    (setf (gethash 'MP acc) (float 58/127))
	    (setf (gethash 'MP+ acc) (float 62/127))
	    (setf (gethash 'MF- acc) (float 66/127))
	    (setf (gethash 'MF acc) (float 71/127))
	    (setf (gethash 'MF+ acc) (float 75/127))
	    (setf (gethash 'F- acc) (float 79/127))
	    (setf (gethash 'F acc) (float 84/127))
	    (setf (gethash 'F+ acc) (float 88/127))
	    (setf (gethash 'FF- acc) (float 92/127))
	    (setf (gethash 'FF acc) (float 97/127))
	    (setf (gethash 'FF+ acc) (float 101/127))
	    (setf (gethash 'FFF- acc) (float 106/127))
	    (setf (gethash 'FFF acc) (float 110/127))
	    (setf (gethash 'FFF+ acc) (float 114/127))
	    (setf (gethash 'FFFF- acc) (float 118/127))
	    (setf (gethash 'FFFF acc) (float 123/127))
	    (setf (gethash 'FFFF+ acc) (float 127/127))
	    (setf (gethash 'MAX acc) 1.0)
	    acc))

(defun defdynamic (name value)
  "Define new dynamic value
name - symbol
value - float between 0 and 1 inclusive"
  (setf (gethash name *dynamics*) value))

(defmethod dynamic ((obj t))
  (cyco-type-error 'dynamic '(float symbol list) obj))

(defmethod dynamic ((n float))
  (float (if (minusp n) -1 (limit n 0.0 1.0))))

(defmethod dynamic ((s symbol))
  (or (gethash s *dynamics*)
      (cyco-value-error 'dynamic S)))

(defmethod dynamic ((lst list))
  (mapcar #'dynamic lst))

(defmethod dynamic ((n number))
  (if (minusp n)
      +rest+
    (float n)))

(defmethod dynamic-p ((n float)) n)

(defmethod dynamic-p ((s symbol))
  (gethash s *dynamics*))

(defmethod dynamic-name ((obj t))
  (cyco-type-error 'dynamic-name '(float symbol list) obj))

(defmethod dynamic-name ((s symbol))
  (or (and (dynamic-p s) s)
      (cyco-value-error 'dynamic-name s)))

(defmethod dynamic-name ((n float))
  (setf n (float n))
  (cond ((not (plusp n)) 'r)
	((>= n 1) 'FFFF+)
	(t (let ((min-diff 1e6)
		 (min-symbol nil))
	     (maphash #'(lambda (a b)
			  (let ((diff (abs (- b n))))
			    (if (< diff min-diff)
				(setf min-diff diff
				      min-symbol a))))
		      *dynamics*)
	     min-symbol))))

(defmethod dynamic-name ((lst list))
  (mapcar #'dynamic-name lst))

(defmethod dynamic->velocity ((obj t))
  (truncate (limit (* 128 (dynamic obj)) 0 127)))

(defmethod dynamic->velocity ((lst list))
  (mapcar #'dynamic->velocity lst))



