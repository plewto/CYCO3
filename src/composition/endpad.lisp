;;;; PigIron CYCO composition endpad
;;;;
;;;; ENDPAD provides a final section to pad-out the end of a MIDI track
;;;; to allow for final note decays..
;;;;

(constant +endpad-properties+ (append
			       +section-properties+
			       '(:shift :render-once)))

(defclass endpad (section) nil)

(defun make-endpad (&key (project *project*)(bars 2) beats )
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
  `(progn
       (banner2 "EndPad")
       (let ((epad (make-endpad :project ,project :bars ,bars :beats ,beats)))
	 (setf *endpad* epad)
	 epad)))

(defmethod clone ((src endpad) &key new-name new-parent)
  (dismiss new-name)
  (make-endpad :project (or new-parent (parent src))
	       :bars (bars src)
	       :beats (beats src)))
  
(defmethod render-once ((epad endpad) &key (offset 0.0))
  (let ((period (phrase-duration epad)))
    (list
     (cons offset (midi-meta-marker "Start ENDPAD"))
     (cons (+ offset period)(midi-note-off 0 0 0)))))

(defmethod render-n ((epad endpad)(n integer) &key (offset))
  (render-once epad :offset offset))


  
