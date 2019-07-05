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
  "Creates Coutin section.
name - Symbol, defaults to 'countin
:project - defaults to *project*
:tempo, :unit, :bars, :beats, :subbeats - Sets metronome time-signature
:instruments - list of instruments ISSUE: What do these instruments do?
:metronome - bool, if t generate metronome events.              
:remarks - optional remarks text."
  (if (not (project-p project))
      (cyco-value-error 'make-countin project
			"project = nil"
			"No current project")
    (let ((cntin (make-instance 'countin
				:name (->symbol (or name "countin"))
				:properties +countin-properties+
				:remarks (->string (or remarks ""))
				:transient t)))
      (put cntin :tempo tempo)
      (put cntin :unit unit)
      (put cntin :bars bars)
      (put cntin :beats beats)
      (put cntin :subbeats subbeats)
      (put cntin :current-part nil)
      (put cntin :groups '())
      (put cntin :instruments instruments)
      (connect project cntin)
      (make-programs 'countin-programs instruments :section cntin)
      (if metronome
	  (make-metronome :name 'countin-metronome
			  :section cntin))
      cntin)))


(defmacro countin (&key 
		     (project *project*)
		     tempo unit bars beats subbeats
		     instruments
		     (metronome t)
		     remarks)
  "Same as make-countin but binds result *countin*"
  `(progn
     (banner2 "Countin")
     (let* ((cntin (make-countin :project ,project
				 :tempo ,tempo
				 :unit ,unit
				 :bars ,bars
				 :beats ,beats
				 :subbeats ,subbeats
				 :instruments ,instruments
				 :metronome ,metronome
				 :remarks ,remarks)))
       (setf *countin* cntin)
       cntin)))

(defmethod clone ((source countin) &key new-name new-parent)
  (let* ((frmt (or new-name "~A"))
	 (name (->symbol (sformat frmt (name source))))
	 (dst (make-countin :name name
			    :project (or new-parent (parent source))
			    :tempo (property source :tempo)
			    :unit (property source :unit)
			    :bars (property source :bars)
			    :beats (property source :beats)
			    :subbeats (property source :subbeats)
			    :instruments (property source :instruments))))
    dst))


