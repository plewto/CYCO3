;;;; CYCO  composition/sections/section
;;;;

(constant +section-properties+
	  (append +time-signature-properties+
		  '(:current-part
		    :cue-function
		    :chord-model
		    :groups
		    :reversible
		    :transposable)))

(defclass section (time-signature) nil
  (:Documentation
   "A Section represents a major division of a composition, such as verse, 
chorus, etc...  The parent of a Section is always a Project and it's 
child nodes are always some type of Part.   A section inherits the default
time-signature from the project but may override any of the time-signature
parameters: tempo, unit, bars, beats, sub-beats and cueing-function.  
Sections also inherit a chord-model from the project but may override it 
with one another model."))

(defmethod section-p ((s section)) t)

(flet ((type-error (parent child)
		   (let ((msg1 "Section child must be a Part.")
			 (msg2 (sformat"~A ~A can not be a child of Section ~A"
				       (type-of child)(name child)(name parent))))
		     (cyco-type-error 'connect 'part child msg1 msg2))))
  
  (defmethod connect ((parent-section section)(child-part t))
    (if (not (part-p child-part))
	(progn 
	  (type-error parent-section child-part)
	  nil)
      (progn
	(call-next-method)
	(init-time-signature child-part)
	(put parent-section :current-part child-part)
	child-part))))


(let ((docstring
        "Creates new Section named name.
:project - Parent project, defaults to *project*
:cuefn - Cueing function, defaults project's value.
:tempo - tempo in BPM, defaults to project's value.
:unit - time signature beat unit, defaults to project's value.
:bars - time signature bars per phrase, defaults to project's value.
:beats - time signature beats per bar, defaults to project's value.
:subbeats - time signature subbeats per beat, defaults to project's value.
:transposable - bool, if nil this Section is immune to transpose and 
and invert operations, default t.
:reservable - bool, if nil this Section is immune to retrograde 
operations, default t.
:remarks - Optional remarks text."))
  (defun make-section (name &key
			    (project *project*)
			    (cuefn nil)
			    (tempo nil)
			    (unit nil)
			    (bars nil)
			    (beats nil)
			    (subbeats nil)
			    (transposable t)
			    (reversible t)
			    (remarks ""))
    docstring
    (if (not (project-p project))
	(cyco-value-error 'make-section project
			  "project = nil"
			  "No current project")
      (let ((section (make-instance 'section
				    :properties +section-properties+
				    :name (->symbol name)
				    :remarks remarks
				    :transient t)))
	(put section :cue-function cuefn)
	(put section :tempo tempo)
	(put section :unit unit)
	(put section :bars bars)
	(put section :beats beats)
	(put section :subbeats subbeats)
	(put section :current-part nil)
	(put section :groups '())
	(put section :transposable transposable)
	(put section :reversible reversible)
	(connect project section)
	(init-time-signature section)
	(put project :current-section section)
	(set-cyco-prompt)
	section))))

(defmacro section (name &key
			(project *project*)
			(cuefn nil)
			(tempo nil)
			(unit nil)
			(bars nil)
			(beats nil)
			(subbeats nil)
			(reversible t)
			(transposable t)
			(remarks ""))
  "Same as make-section except binds new section to name symbol name."
  `(progn
     (banner2 (sformat "Section ~A" ',name))
     (if (not (symbolp ',name))
	 (cyco-type-error 'section 'symbol ',name
			  "Do not quote section name.")
       (let ((section (make-section ',name
			      :project ,project
			      :cuefn ,cuefn
			      :tempo ,tempo 
			      :unit ,unit 
			      :bars ,bars 
			      :beats ,beats 
			      :subbeats ,subbeats
			      :transposable ,transposable
			      :reversible ,reversible
			      :remarks ,remarks)))
	 (defparameter ,name section)
	 section))))

(defmethod groups ((s section))
  (property s :groups))

(defmethod has-group-p ((s section)(group-name symbol))
  (car (member group-name (groups s)
	       :test #'(lambda (a b)(eq a (name b))))))

(defmethod add-group ((s section)(grp group))
  (if (not (has-group-p s (name grp)))
      (put s :groups
	    (cons grp (property s :groups)))))

(defmethod mute-all ((s section))
  (dolist (grp (property s :groups))
    (mute grp :mute))
  (dolist (prt (children s))
    (mute prt :mute)))

(defmethod unmute-all ((s section))
  (dolist (grp (property s :groups))
    (mute grp :unmute))
  (dolist (prt (children s))
    (mute prt :unmute)))

(defmethod print-tree ((s section) &optional (depth 0))
  (call-next-method)
  (let ((gtab (spaces (* 4 (1+ depth)))))
    (dolist (grp (groups s))
      (format t "~AGroup ~16A State ~7A : " gtab (name grp)(mute-state grp))
      (dolist (prt (group-members grp))
	(format t "~A " (name prt)))
      (format t "~%"))))

(labels ((clone-groups (source-section destination-section)
		       (dolist (grp (property source-section :groups))
			 (clone-group destination-section grp)))

	 ;; ISSUE: member copy will not work if names differ
	 ;;        between source and destination 
	 ;;
	 (clone-group (destination-group group)
	  (let* ((member-names (let ((acc '()))
				 (dolist (prt (group-members group))
				   (push (name prt) acc))
				 (reverse acc)))
		 (new-group (make-group (name group)
					:member-names member-names
					:section destination-group)))
	    (setf (mute-state new-group)(mute-state group)))))

  (defmethod clone ((source-section section) &key new-name new-parent)
    (let* ((frmt (or new-name "~A"))
	   (name (->symbol (sformat frmt (name source-section))))
	   (parent (or new-parent (parent source-section)))
	   (new-section (make-section name
			      :project parent
			      :cuefn (property source-section :cue-function)
			      :remarks (remarks source-section))))
      (put new-section :chord-model (property source-section :chord-model))
      (copy-time-signature source-section new-section)
      (dolist (c (children source-section))
	(let ((prt (clone c :new-name "~A" :new-parent new-section)))
	  (put prt :muted (property c :muted))))
      (clone-groups source-section new-section)
      new-section)))

(defmethod transpose ((s section)(n t))
  (if (property s :transposable)
      (dolist (prt (children s))
	(transpose prt n)))
  s)

(defmethod invert ((s section)(pivot t))
  (if (and pivot (property s :transposable))
      (dolist (prt (children s))
	(invert prt pivot)))
  s)

(defmethod retrograde ((s section))
  (if (property s :reversible)
      (dolist (c (children s))
	(retrograde c)))
  s)

(defmethod render-once ((section section) &key (offset 0.0))
  (let* ((event-list (list (cons offset
				 (midi-meta-marker (sformat "Start Section ~A" (name section))))))
	 (period (phrase-duration section))
	 (end-mask (+ offset period)))
    (dolist (part (reverse (children section)))
      (let* ((count (truncate (/ period (phrase-duration part))))
	     (shift (or (property part :shift) 0.0)))
	(dolist (event (render-n part count :offset (+ offset shift)))
	  (let* ((time (car event))
		 (message (cdr event)))
	    (if (or (< time end-mask)(not (midi-note-on-p message)))
		(push (clone event) event-list))))))
    (sort-midi-events event-list)))


(defmethod render-n ((section section)(n integer) &key (offset 0.0))
  (let ((event-list '())
	(period (phrase-duration section))
	(template (render-once section)))
    (dotimes (i n)
      (dolist (event template)
	(let ((relative-time (car event))
	      (message (cdr event)))
	  (push (cons (+ offset (* i period) relative-time) message) event-list))))
    (sort-midi-events event-list)))

(defmethod dump-events ((s section) &key
			(range (cons 0.0 1e9))
			(filter #'false)
			(render nil))
  (dump-events (render-once s) :range range :filter filter :render render))

(let ((docstring
       "Creates MIDI filename used to save section.  
Using the defaults with a project name 'foo and section name 'alpha
the filename is <user-home>/cyco-projects/foo/MIDI/alpha.mid

:section - sets an explicit section, defaults to the current section 
of *project*
:fname - sets an alternate filename.  If fname is an absolute position 
the it is used directly, otherwise the value of fname replaces the 
section name in the example above.   In all cases a .mid extension
is appended to the name if needed."   ))

  (defun section-filename (&key section fname)
    docstring
    (if (absolute-path-p fname)
	(return-from section-filename fname))
    (let* ((sec (or (and (section-p section) section)
		    (and (project-p *project*)
			 (property *project* :current-section))
		    (progn
		      (cyco-composition-error 'section-filename "No default project")
		      (return-from section-filename nil))))
	   (project (parent sec))
	   (project-directory (property project :project-directory))
	   (project-name (string-downcase (name project)))
	   (output-directory (property project :output-directory))
	   (section-name (string-downcase (or fname (name sec))))
	   (fqn (join-path-list (list project-directory project-name output-directory section-name) :as-file)))
      (append-filename-extension fqn ".mid"))))
	       

(defmethod section->smf ((section section) &key (offset 0.0)(repeat 1))
  "Creates Standard MIDI File from Section contents."
  (let* ((events (render-n section repeat :offset offset))
	 (track (make-instance 'smf-track :events events))
	 (midi-file (smf :format 1 :track-count 1)))
    (setf (aref (smf-tracks midi-file) 0) track)
    midi-file))

(defmethod ->midi ((section section) &key (filename nil)(offset 0.0)(repeat 1)(pad 2.0))
  "Write section contents to Standard MIDI file."
  (let* ((midi-file (section->smf section :offset offset :repeat repeat))
	 (output-filename (section-filename :section section :fname filename)))
    (write-smf midi-file output-filename :pad pad)
    midi-file))


