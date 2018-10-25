;;;; CYCO3 src/composition/countin
;;;;
;;;; Countin provides MIDI initialization and metronome count-in.
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

(defmethod clone ((src countin) &key new-name new-parent)
  (let* ((frmt (or new-name "~A"))
	 (name (->symbol (sformat frmt (name src))))
	 (dst (make-countin :name name
			    :project (or new-parent (parent src))
			    :tempo (property src :tempo)
			    :unit (property src :unit)
			    :bars (property src :bars)
			    :beats (property src :beats)
			    :subbeats (property src :subbeats)
			    :instruments (property src :instruments))))
    dst))
			    

