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

(global *dynamic-table*
	  (let ((dynamic-table (make-hash-table :size 40)))
	    (setf (gethash 'R dynamic-table) -1.0)
	    (setf (gethash 'MIN dynamic-table) (float 1/127))
	    (setf (gethash 'PPPP- dynamic-table) (float 2/127))
	    (setf (gethash 'PPPP dynamic-table) (float 6/127))
	    (setf (gethash 'PPPP+ dynamic-table) (float 10/127))
	    (setf (gethash 'PPP- dynamic-table) (float 14/127))
	    (setf (gethash 'PPP dynamic-table) (float 19/127))
	    (setf (gethash 'PPP+ dynamic-table) (float 23/127))
	    (setf (gethash 'PP- dynamic-table) (float 27/127))
	    (setf (gethash 'PP dynamic-table) (float 32/127))
	    (setf (gethash 'PP+ dynamic-table) (float 36/127))
	    (setf (gethash 'P- dynamic-table) (float 40/127))
	    (setf (gethash 'P dynamic-table) (float 45/127))
	    (setf (gethash 'P+ dynamic-table) (float 49/127))
	    (setf (gethash 'MP- dynamic-table) (float 53/127))
	    (setf (gethash 'MP dynamic-table) (float 58/127))
	    (setf (gethash 'MP+ dynamic-table) (float 62/127))
	    (setf (gethash 'MF- dynamic-table) (float 66/127))
	    (setf (gethash 'MF dynamic-table) (float 71/127))
	    (setf (gethash 'MF+ dynamic-table) (float 75/127))
	    (setf (gethash 'F- dynamic-table) (float 79/127))
	    (setf (gethash 'F dynamic-table) (float 84/127))
	    (setf (gethash 'F+ dynamic-table) (float 88/127))
	    (setf (gethash 'FF- dynamic-table) (float 92/127))
	    (setf (gethash 'FF dynamic-table) (float 97/127))
	    (setf (gethash 'FF+ dynamic-table) (float 101/127))
	    (setf (gethash 'FFF- dynamic-table) (float 106/127))
	    (setf (gethash 'FFF dynamic-table) (float 110/127))
	    (setf (gethash 'FFF+ dynamic-table) (float 114/127))
	    (setf (gethash 'FFFF- dynamic-table) (float 118/127))
	    (setf (gethash 'FFFF dynamic-table) (float 123/127))
	    (setf (gethash 'FFFF+ dynamic-table) (float 127/127))
	    (setf (gethash 'MAX dynamic-table) 1.0)
	    dynamic-table))

(defun defdynamic (name value)
  "Define new dynamic value
name - symbol
value - float between 0 and 1 inclusive"
  (setf (gethash name *dynamic-table*) value))

(defmethod dynamic ((object t))
  (cyco-type-error 'dynamic '(float symbol list) object))

(defmethod dynamic ((dynamic-value float))
  (float (if (minusp dynamic-value) -1 (limit dynamic-value 0.0 1.0))))

(defmethod dynamic ((dynamic-symbol symbol))
  (or (gethash dynamic-symbol *dynamic-table*)
      (cyco-value-error 'dynamic dynamic-symbol)))

(defmethod dynamic ((dynamic-list list))
  (mapcar #'dynamic dynamic-list))

(defmethod dynamic ((dynamic-value number))
  (if (minusp dynamic-value)
      +rest+
    (float dynamic-value)))

(defmethod dynamic-p ((n float)) n)

(defmethod dynamic-p ((symbol symbol))
  (gethash symbol *dynamic-table*))

(defmethod dynamic-name ((object t))
  (cyco-type-error 'dynamic-name '(float symbol list) object))

(defmethod dynamic-name ((dynamic-symbol symbol))
  (or (and (dynamic-p dynamic-symbol) dynamic-symbol)
      (cyco-value-error 'dynamic-name dynamic-symbol)))

(defmethod dynamic-name ((dynamic-value float))
  (setf dynamic-value (float dynamic-value))
  (cond ((not (plusp dynamic-value)) 'r)
	((>= dynamic-value 1) 'FFFF+)
	(t (let ((min-diff 1e6)
		 (min-symbol nil))
	     (maphash #'(lambda (a b)
			  (let ((diff (abs (- b dynamic-value))))
			    (if (< diff min-diff)
				(setf min-diff diff
				      min-symbol a))))
		      *dynamic-table*)
	     min-symbol))))

(defmethod dynamic-name ((dynamic-value-list list))
  (mapcar #'dynamic-name dynamic-value-list))

(defmethod dynamic->velocity ((dynamic-value t))
  (truncate (limit (* 128 (dynamic dynamic-value)) 0 127)))

(defmethod dynamic->velocity ((dynamic-value-list list))
  (mapcar #'dynamic->velocity dynamic-value-list))



