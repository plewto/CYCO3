;;;; cyco chords/fretworks/monochord
;;;; 

;; (defgeneric mc-position->keynumber (mc pos)
;;   (:documentation
;;    "Returns corresponding keynumber for position pos of monochord.
;; Returns +REST+ for negative positions or positions greter then fret-count."))

(defclass monochord nil
  ((fret-count
    :type integer
    :reader fret-count
    :initform nil
    :initarg :frets)
   (root-key
    :type integer
    :reader monochord-root
    :initform nil
    :initarg :root)))

(defmethod ->string ((mc monochord))
  (sformat "MONOCHORD(~A) root ~A"
	   (fret-count mc)
	   (monochord-root mc)))

(defmethod mc-position->keynumber ((mc monochord)(pos t))
  (cond
   ((eq pos 'O) 0)
   ((eq pos 'X) +REST+)
   ((not (integerp pos)) :ERROR)
   ((minusp pos) +REST+)
   ((> pos (fret-count mc)) +REST+)
   (t (+ (monochord-root mc) pos))))

(defmethod fm-key-positions ((mc monochord)(key t) &key (no-duplicates nil))
  (dismiss no-duplicates)
  (let* ((pc (pitch-class key))
	 (rpc (pitch-class (monochord-root mc)))
	 (diff (- pc rpc))
	 (pos (if (minusp diff)(+ 12 diff) diff))
	 (acc '()))
    (while (< pos (fret-count mc))
      (push pos acc)
      (setf pos (+ 12 pos)))
    (reverse acc)))
 
