;;;; CYCO
;;;;
;;;; ENDPAD provides a final section to "pad-out" the end of a MIDI track.
;;;; They are useful to provide space for final decay tails.

(constant +endpad-properties+ (append
			       +section-properties+
			       '(:shift :render-once)))

(defclass endpad (section) nil
  (:documentation
   "ENDPAD is a specialized Section used to extend the end time of a project."))

(defun make-endpad (&key (project *project*)(bars 2) beats )
  "Creates ENDPAD section"
  (setf project (or project *project*))
  (if (not (project-p project))
      (cyco-value-error 'endpad project
			"project = nil"
			"No current project")
    (let ((epad (make-instance 'endpad
			       :name 'endpad
			       :properties +endpad-properties+
			       :remarks "Pads track ending."
			       :transient t)))
      (put epad :unit 'q)
      (put epad :bars bars)
      (put epad :beats beats)
      (put epad :shift 0.0)
      (put epad :render-once t)
      (put epad :current-part nil)
      (put epad :groups '())
      (connect project epad)
      (setf *endpad* epad)
      epad)))

(defmacro endpad (&key project bars beats)
  "Same as make-endpad except binds result to symbol named name."
  `(progn
       (banner2 "EndPad")
       (let ((epad (make-endpad :project ,project :bars ,bars :beats ,beats)))
	 (setf *endpad* epad)
	 epad)))

(defmethod clone ((source endpad) &key new-name new-parent)
  (dismiss new-name)
  (make-endpad :project (or new-parent (parent source))
	       :bars (bars source)
	       :beats (beats source)))
  
(defmethod render-once ((endpad endpad) &key (offset 0.0))
  (let ((period (phrase-duration endpad)))
    (list
     (cons offset (midi-meta-marker "End Pad"))
     (cons (+ offset period)(midi-end-of-track)))))

(defmethod render-n ((endpad endpad)(n integer) &key (offset))
  (render-once endpad :offset offset))


