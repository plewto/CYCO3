;;;; CYCO  composition/sections/section
;;;;

(constant +section-properties+
	  (append +time-signature-properties+
		  '(:current-part
		    :cue-function
		    :shuffle-function
		    :chord-model
		    :groups
		    :reversible
		    :transposable
		    :midi-filename)))

(defclass section (time-signature) nil
  (:Documentation
   "A Section represents a major composition division, IE verse, chorus,
bridge etc...  The parent of a Section is always a Project and its 
child nodes are always some type of Part.   A section inherits time-signature
and chord-model parameters from the project but may selectively override them."))

(defmethod section-p ((s section)) t)

(param *->midi-hook* #'(lambda (filename)
			 (format t "->MIDI Hook function executed, filename: ~S~%" filename)))


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

(defun make-section (name &key
			  (project *project*)
			  (cuefn nil)
			  (shuffle nil)
			  (tempo nil)
			  (unit nil)
			  (bars nil)
			  (beats nil)
			  (subbeats nil)
			  (transposable t)
			  (reversible t)
			  (remarks ""))
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
      (put section :shuffle-function shuffle)
      (put section :tempo tempo)
      (put section :unit unit)
      (put section :bars bars)
      (put section :beats beats)
      (put section :subbeats subbeats)
      (put section :current-part nil)
      (put section :groups '())
      (put section :transposable transposable)
      (put section :reversible reversible)
      (put section :midi-filename nil)
      (connect project section)
      (init-time-signature section)
      (put project :current-section section)
      section)))

(defmacro section (name &key
			(project *project*)
			(cuefn nil)
			(shuffle nil)
			(tempo nil)
			(unit nil)
			(bars nil)
			(beats nil)
			(subbeats nil)
			(reversible t)
			(transposable t)
			(remarks "")
			(auto-prune t))
  `(progn
     (banner2 (sformat "Section ~A" ',name))
     (if (not (symbolp ',name))
	 (cyco-type-error 'section 'symbol ',name
			  "Do not quote section name.")
       (progn
	 (if ,auto-prune (prune-project ',name :project ,project))
	 (let ((section (make-section ',name
				      :project ,project
				      :cuefn ,cuefn
				      :shuffle ,shuffle
				      :tempo ,tempo 
				      :unit ,unit 
				      :bars ,bars 
				      :beats ,beats 
				      :subbeats ,subbeats
				      :transposable ,transposable
				      :reversible ,reversible
				      :remarks ,remarks)))
	   (defparameter ,name section)
	   section)))))

(let* ((function-docstring
        "Creates new Section.
MAKE-SECTION (function) and SECTION (macro) are nearly identical.  The
primary differences is the macro binds the new section to a symbol with the
same name.

:project - Parent project, defaults to *project*
:cuefn   - Cueing function, defaults project's value.
:shuffle - Shuffle function, defaults to project's value.
:tempo   - tempo in BPM, defaults to project's value.
:unit    - time signature beat unit, defaults to project's value.
:bars    - time signature phrase length, defaults to project's value.
:beats   - time signature beats per bar, defaults to project's value.
:subbeats - time signature subbeats per beat, defaults to project's value.
:transposable - bool, if nil the Section is immune to transpose and 
                and invert operations, default t.
:reservable   - bool, if nil this Section is immune to retrograde 
                operations, default t.
:remarks - Optional remarks text.")
      (macro-docstring (str+ function-docstring "
:auto-prune  - bool, if true removes any existing section with the same
               name from the project.")))
  (setf (documentation 'make-section 'function) function-docstring)
  (setf (documentation 'section 'function) macro-docstring))


(defmethod groups ((section section))
  (property section :groups))

(defmethod has-group-p ((section section)(group-name symbol))
  (car (member group-name (groups section)
	       :test #'(lambda (a b)(eq a (name b))))))

(defmethod add-group ((section section)(group group))
  (if (not (has-group-p section (name group)))
      (put section :groups
	   (cons group (property section :groups)))))

(defmethod mute-all ((section section))
  (dolist (group (property section :groups))
    (mute group :mute))
  (dolist (part (children section))
    (mute part :mute)))

(defmethod unmute-all ((section section))
  (dolist (group (property section :groups))
    (mute group :unmute))
  (dolist (part (children section))
    (mute part :unmute)))

(defmethod print-tree ((section section) &optional (depth 0))
  (call-next-method)
  (let ((group-tab (spaces (* 4 (1+ depth)))))
    (dolist (group (groups section))
      (format t "~AGroup ~16A State ~7A : " group-tab (name group)(mute-state group))
      (dolist (part (group-members group))
	(format t "~A " (name part)))
      (format t "~%"))))

(defun bulk-rename-parts (section prefix-trim new-prefix)
  "Renames all section parts.
section     - The section.
prefix-trim - Number of characters to remove from the left of each name.
new-prefix  - prefix added to each name."
  (dolist (child (remove-if-not #'part-p (children section)))
    (let* ((old-name (->string (name child)))
	   (suffix (subseq old-name prefix-trim))
	   (new-name (str+ new-prefix suffix)))
      (set-name child (->symbol new-name)))))


(labels ((exclude-parts (mother daughter child-list)
			   (dolist (name (->list child-list))
		    (let ((child (find-child daughter name)))
		      (if child
			  (disconnect child)
			(cyco-error (sformat "Clone section ~A exclude list contains non-existent child: ~A"
					     (name mother) name))))))
	 (bulk-rename (mother daughter)
		      (let ((trim (length (sformat "~A" (name mother))))
			    (prefix (name daughter)))
			(bulk-rename-parts daughter trim prefix)))

	 (bind-parts (daughter)
		     (dolist (child (children daughter))
		       (let ((sym (->symbol (name child))))
			 (set sym child)))))

	(defmethod clone ((mother section) &key new-name new-parent exclude
			  (rename-parts t)(bind t))
	  "Makes clone of section.
:new-name symbol, if non-nil assigns symbol as the name of the resulting section.
:new-parent If non-nil sets the resulting section as a child of new-parent.
By default the cloned section has the same parent as the original.
:exclude List of part names to be excluded from the cloned copy.
:rename-parts If non-nil all of the parts in the cloned copy will be renamed.
:bind If true all of the parts in the cloned copy will be bound to symbols eq to their names.

NOTE: The new clone becomes the projects 'current-section'."

	  (let* ((frmt (or new-name "~A"))
		 (name (->symbol (sformat frmt (name mother))))
		 (parent (or new-parent (parent mother)))
		 (daughter (make-section name
					 :project parent
					 :cuefn (property mother :cue-function)
					 :shuffle (property mother :shuffle-function)
					 :transposable (property mother :transposable)
					 :reversible (property mother :reversible)
					 :remarks (remarks mother))))
	    (copy-time-signature mother daughter)
	    (dolist (child (children mother))
	      (let ((child-daughter (clone child :new-parent daughter)))
	    	(copy-time-signature child child-daughter)))
	    (exclude-parts mother daughter exclude)
	    (if rename-parts (bulk-rename mother daughter))
	    (if bind (bind-parts daughter))
	    (reset daughter)
	    daughter)))

(defmethod transpose ((section section)(n t))
  (if (property section :transposable)
      (dolist (part (children section))
	(transpose part n)))
  section)

(defmethod invert ((section section)(pivot t))
  (if (and pivot (property section :transposable))
      (dolist (part (children section))
	(invert part pivot)))
  section)

(defmethod retrograde ((section section))
  (if (property section :reversible)
      (dolist (part (children section))
	(retrograde part)))
  section)

(defmethod render-once ((section section) &key (offset 0.0) &allow-other-keys)
  (let* ((event-list (list (cons offset
				 (midi-meta-marker (name section)))
			   (cons offset
				 (midi-tempo-message (tempo section)))
			   (cons offset
				 (midi-time-signature (beats section)(unit section)))))
	 (period (phrase-duration section)))
    (dolist (part (reverse (children section)))
      (let* ((count (truncate (/ period (phrase-duration part)))))
	(dolist (event (render-n part count :offset offset))
	  (push (clone event) event-list))))
    (sort-midi-events event-list)))

(defmethod render-n ((section section)(n integer) &key (offset 0.0) &allow-other-keys)
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
	       
(defmethod section->smf ((section section) &key (offset 0.0)(repeat 1)(no-stripe nil))
  "Creates Standard MIDI File from Section contents."
  (declare (ignore no-stripe))
  (let* ((events (render-n section repeat :offset offset))
	 (track (make-instance 'smf-track :events events))
	 (midi-file (smf :format 1 :track-count 1)))
    (setf (aref (smf-tracks midi-file) 0) track)
    midi-file))

(defmethod ->midi ((section section) &key (filename nil)(offset 0.0)(repeat 1)(pad 2.0)(no-stripe nil))
  "Write section contents to Standard MIDI file."
  (declare (ignore no-stripe))
  (let* ((midi-file (section->smf section :offset offset :repeat repeat))
	 (output-filename (section-filename :section section :fname filename)))
    (write-smf midi-file output-filename :pad pad)
    (put section :midi-filename output-filename)
    (funcall *->midi-hook* output-filename)
    midi-file))

(defun  get-section-part (section part-name)
  "Returns the named part from section.
It is a CYCO-ERROR if the part does not exists."
  (or (find-child section part-name)
      (cyco-error
       (sformat "Section ~A does not contain part named: ~A" (name section) part-name))))




(constant +NULL-SECTION+ (make-instance 'section
					:name 'NULL-SECTION
					:properties +SECTION-PROPERTIES+))

(labels ((unmute-all (section)
		      (dolist (g (groups section))
			(mute g :unmute))
		      (dolist (c (children section))
			(mute c :unmute)))
	  
	  (split-channels (section &key (unmute-all t))
		    (if unmute-all (unmute-all section))
		    (let ((events (render-once section))
			  (acc (->vector (copies 16))))
		      (dotimes (ci 16)
			(let ((bcc '()))
			  (dolist (evn events)
			    (let ((msg (cdr evn)))
			      (if (midi-channel-message-p msg)
				  (if (eq (channel-index msg) ci)
				      (push evn bcc))
				(push evn bcc))))
			  (setf (aref acc ci) (reverse bcc))))
		      acc)))

	(defmethod partition ((sec section) &key (unmute-all t) &allow-other-keys)
	  "Render Section to individual MIDI channels.
Creates one file for each non-empty channel."
	  (let ((sources (split-channels sec :unmute-all unmute-all))
		(directory (car (split-path (section-filename :section sec)))))
	    (dotimes (ci 16)
	      (let* ((c (1+ ci))
		     (c-events (aref sources ci))
		     (fname (sformat "~A__CHAN_~2,'0D.mid" (name sec) c))
		     (filename (join-path directory fname :as-file)))
		(if (some #'(lambda (event)(midi-channel-message-p (cdr event))) c-events)
		    (let ((trk (make-instance 'smf-track))
			  (smf (smf)))
		      (setf (aref (smf-tracks smf) 0) trk)
		      (dolist (event c-events)
			(push-event (car event)(cdr event) trk))
		      (format t "Sec: ~A  Chan: ~02D  Count: ~4D : " (name sec) c (length c-events))
		      (write-smf smf filename))))))))

(defmethod reset ((s section))
  (dolist (part (children s))
    (reset part))
  s)
