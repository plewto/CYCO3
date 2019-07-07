;;;; CYCO
;;;;
;;;; Coutin is a specialized Section for MIDI initialization and
;;;; metronome count-in.
;;;;

(constant +countin-properties+
	  (append +section-properties+
		  '(:instruments)))

(defclass countin (section) nil)

(let ((docstring 
 "Creates Coutin section.
:name        - Symbol, defaults to 'countin
:project     - Defaults to *project*
:tempo       - Defaults to project tempo
:unit        - Defaults to project time-signature unit
:bars        - Defaults to project time-signature phrase-length in bars
:beats       - Defaults to project time-signature beats per bar
:subbeats    - Defaults to project time-signature sub beats per beat.
:metronome   - bool, if t generate metronome events.
:instruments - list of instruments ISSUE: What do these instruments do?              
:remarks     - optional remarks text."))
  
  (defun make-countin (&key
		       name
		       (project *project*)
		       tempo unit bars beats subbeats
		       instruments
		       (metronome t)
		       remarks)
    docstring
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
	countin))))
  

(defmacro countin (&key 
		     (project *project*)
		     tempo unit bars beats subbeats
		     instruments
		     (metronome t)
		     remarks)
  "Same as make-countin but binds result *countin*"
  `(progn
     (banner2 "Countin")
     (let* ((countin (make-countin :project ,project
				   :tempo ,tempo
				   :unit ,unit
				   :bars ,bars
				   :beats ,beats
				   :subbeats ,subbeats
				   :instruments ,instruments
				   :metronome ,metronome
				   :remarks ,remarks)))
       (setf *countin* countin)
       countin)))

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

