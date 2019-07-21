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

(defmacro endpad (&key project bars beats (auto-prune t))
  `(progn
     (banner2 "ENDPAD")
     (if ,auto-prune (prune-project 'endpad) :project ,project)
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


(let* ((function-docstring
	"MAKE-ENDPAD (function) & ENDPAD (macro) creates new endpad section.

An endpad is an empty terminal section of a project to provide time
for final decay tails.   The new section has the name 'ENDPAD and the 
macro also binds the new section to the symbol 'ENDPAD.

:project  - The project
:bars     - Number of bars, defaults to project
:beats    - Number of beats per bar, defaults to project")
       (macro-docstring (str+ function-docstring "
:auto-prune - bool, if true, first remove any existing endpad from the project")))
  (setf (documentation 'make-endpad 'function) function-docstring)
  (setf (documentation 'endpad 'function) macro-docstring))
  


