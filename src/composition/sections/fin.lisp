;;;; CYCO composition/section fin.lisp
;;;;
;;;; Specialized final section, used to pad-out end of MIDI tracks with
;;;; a few moments of silence.
;;;;

(in-package :cyco)

(constant +fin-properties+ (append +section-properties+
				   '(:shift :render-once)))

(defclass fin (section) nil)

(defun make-fin (name &key (project *project*) bars beats)
  (let ((section (make-instance 'fin
				:name name
				:properties +fin-properties+
				:remarks "Pads track ending with silence."
				:transient t)))
    (put section :unit 'q)
    (put section :bars (or bars 2))
    (put section :beats beats)
    (put section :shift 0.0)
    (put section :render-once t)
    (put section :current-part nil)
    (put section :groups '())
    (connect project section)
    (init-time-signature section)
    section))

(defmacro fin (name &key (project *project*) bars beats (auto-prune t))
  `(progn
     (banner2 (->string ',name))
     (if ,auto-prune (prune-project ',name :project ,project))
     (let ((section (make-fin ',name :project ,project :bars ,bars :beats ,beats)))
       (defparameter ,name section)
       section)))

(defmethod clone ((mother fin) &key new-name new-parent)
  (let* ((frmt (or new-name "~A"))
	 (name (sformat frmt (name mother)))
	 (parent (or new-parent (parent mother)))
	 (daughter (make-fin name :project parent
			     :bars (bars mother)
			     :beats (beats mother))))
    daughter))


(defmethod render-once ((section fin) &key (offset 0.0) &allow-other-keys)
  (let* ((period (phrase-duration section))
	 (event-list (list (cons offset (midi-meta-marker "FIN"))
			   (cons (+ offset period)(midi-end-of-track)))))
    (sort-midi-events event-list)))


(defmethod render-n ((section fin)(n integer) &key (offset 0.0) &allow-other-keys)
  (let* ((period (phrase-duration section))
	 (terminal-time (+ offset (* period n)))
	 (event-list (list (cons offset (midi-meta-marker "FIN"))
			   (cons terminal-time (midi-end-of-track)))))
    (sort-midi-events event-list)))
