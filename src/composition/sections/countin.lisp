;;;; CYCO
;;;;
;;;; Coutin is a specialized Section for MIDI initialization and
;;;; metronome count-in.
;;;;

(constant +countin-properties+
	  (append +section-properties+
		  '(:instruments)))

(defclass countin (section) nil)



  
(defun make-countin (&key
		     name
		     (project *project*)
		     tempo unit bars beats subbeats
		     instruments
		     (metronome t)
		     remarks)
  (if (not (project-p project))
      (cyco-value-error 'make-countin project
			"project = nil"
			"No current project")
    (let ((countin (make-instance 'countin
				  :name (->symbol (or name "countin"))
				  :properties +countin-properties+
				  :remarks (->string (or remarks ""))
				  :transient t)))
      (put countin :tempo tempo)
      (put countin :unit unit)
      (put countin :bars bars)
      (put countin :beats beats)
      (put countin :subbeats subbeats)
      (put countin :current-part nil)
      (put countin :groups '())
      (put countin :instruments instruments)
      (connect project countin)
      (make-programs 'countin-programs instruments :section countin)
      (if metronome
	  (make-metronome :name 'countin-metronome
			  :section countin))
      (if instruments
	  (make-programs 'countin-programs (->list instruments) :time '(1 1 1)
			 :section countin))
      countin)))

(defmacro countin (&key (project *project*)
			tempo unit bars beats subbeats
			instruments
			(metronome t)
			remarks
			(auto-prune t))
  `(progn
     (banner2 "Countin")
     (if ,auto-prune (prune-project 'countin :project ,project))
     (let* ((new-countin (make-countin :project ,project
				       :tempo ,tempo
				       :unit ,unit
				       :bars ,bars
				       :beats ,beats
				       :subbeats ,subbeats
				       :instruments ,instruments
				       :metronome ,metronome
				       :remarks ,remarks)))
       (param countin new-countin)
       new-countin)))


(defmethod clone ((source countin) &key new-name new-parent)
  (let ((name (->symbol (sformat (or new-name "~A") (name source)))))
    (make-countin :name name
		  :project (or new-parent (parent source))
		  :tempo (property source :tempo)
		  :unit (property source :unit)
		  :bars (property source :bars)
		  :beats (property source :beats)
		  :subbeats (property source :subbeats)
		  :instruments (property source :instruments))))



(let* ((function-docstring
       "MAKE-COUNTIN & COUNTIN  Creates new countin section.

MAKE-COUNTIN (function) and COUNTIN (macro) have nearly identical usage.
The main difference is that countin binds the new section to the symbol 
'COUNTIN while MAKE-COUNTIN does not.

:name        - Symbol, defaults to 'countin
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
:auto-prune  - bool, if true, first remove and existing countin from the project.")))
   (setf (documentation 'make-countin 'function) function-docstring)
   (setf (documentation 'countin 'function) macro-docstring))
