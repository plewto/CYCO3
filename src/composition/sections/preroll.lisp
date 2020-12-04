;;;; CYCO composition/section preroll.lisp
;;;;
;;;; preroll is a specialized Section for MIDI initialization and
;;;; metronome count-in.
;;;;

(in-package :cyco)

(constant +preroll-properties+
	  (append +section-properties+
		  '(:instruments)))

(defclass preroll (section) nil)
  
(defun make-preroll (&key
		     name
		     (project *project*)
		     tempo unit bars beats subbeats
		     instruments
		     (metronome t)
		     remarks)
  (if (not (project-p project))
      (cyco-value-error 'make-preroll project
			"project = nil"
			"No current project")
    (let ((preroll (make-instance 'preroll
				  :name (->symbol (or name "preroll"))
				  :properties +preroll-properties+
				  :remarks (->string (or remarks ""))
				  :transient t)))
      (put preroll :tempo tempo)
      (put preroll :unit unit)
      (put preroll :bars bars)
      (put preroll :beats beats)
      (put preroll :subbeats subbeats)
      (put preroll :current-part nil)
      (put preroll :groups '())
      (put preroll :instruments instruments)
      (connect project preroll)
      (if metronome
	  (make-metronome 'preroll-metronome
			  :section preroll))
      (if instruments
	  (make-programs 'preroll-programs (->list instruments) :time '(1 1 1)
			 :section preroll))
      preroll)))

(defmacro preroll (&key (project *project*)
			tempo unit bars beats subbeats
			instruments
			(metronome t)
			remarks
			(auto-prune t))
  `(progn
     (banner2 "Preroll")
     (if ,auto-prune (prune-project 'preroll :project ,project))
     (let* ((new-preroll (make-preroll :project ,project
				       :tempo ,tempo
				       :unit ,unit
				       :bars ,bars
				       :beats ,beats
				       :subbeats ,subbeats
				       :instruments ,instruments
				       :metronome ,metronome
				       :remarks ,remarks)))
       (param preroll new-preroll)
       new-preroll)))

(let* ((function-docstring
       "MAKE-PREROLL & PREROLL  Creates new preroll section.

MAKE-PREROLL (function) and PREROLL (macro) have nearly identical usage.
The main difference is that preroll binds the new section to the symbol 
'PREROLL while MAKE-PREROLL does not.

:name        - Symbol, defaults to 'preroll
:project     - Defaults to *project*
:tempo       - Defaults to project tempo
:unit        - Defaults to project time-signature unit
:bars        - Defaults to project time-signature phrase-length in bars
:beats       - Defaults to project time-signature beats per bar
:subbeats    - Defaults to project time-signature sub beats per beat.
:metronome   - bool, if t generate metronome events.
:instruments - list of instruments.  Program change events are generated
               for each instrument
:remarks     - optional remarks text.")

       (macro-docstring (str+ function-docstring "
:auto-prune  - bool, if true, first remove and existing preroll from the project.")))
   (setf (documentation 'make-preroll 'function) function-docstring)
   (setf (documentation 'preroll 'function) macro-docstring))
