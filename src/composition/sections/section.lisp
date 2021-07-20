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
		    :transposable)))

(defclass section (time-signature) nil
  (:Documentation
   "A Section represents a major composition division, IE verse, chorus,
bridge etc...  The parent of a Section is always a Project and its 
child nodes are always some type of Part.   A section inherits time-signature
and chord-model parameters from the project but may selectively override them."))

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
      (connect project section)
      (init-time-signature section)
      (put project :current-section section)
      (set-cyco-prompt)
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

(defmethod clone ((mother section) &key new-name new-parent)
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
    (reset daughter)
    daughter))
	 
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

(defmethod stripe-section ((section section)(event-list list) &key (offset 0.0)(count 1))
  (let* ((quarter (beat-duration section))
	(division 24.0)
	(time-delta (/ quarter division))
	(time offset)
	(end-time (+ offset (* count (duration section)))))
    (while (< time end-time)
      (push (cons time (midi-clock)) event-list)
      (setf time (+ time time-delta)))
    event-list))

(defmethod render-once ((section section) &key (offset 0.0)(stripe t) &allow-other-keys)
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
    (setf event-list (if stripe
			 (stripe-section section event-list :offset offset)
		       event-list))
    (sort-midi-events event-list)))

(defmethod render-n ((section section)(n integer) &key (offset 0.0)(stripe t) &allow-other-keys)
  (let ((event-list '())
	(period (phrase-duration section))
	(template (render-once section :stripe nil)))
    (dotimes (i n)
      (dolist (event template)
	(let ((relative-time (car event))
	      (message (cdr event)))
	  (push (cons (+ offset (* i period) relative-time) message) event-list))))
    (setf event-list (if stripe
			 (stripe-section section event-list :offset offset :count n)
		       event-list))
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
  (let* ((events (render-n section repeat :offset offset :no-stripe no-stripe))
	 (track (make-instance 'smf-track :events events))
	 (midi-file (smf :format 1 :track-count 1)))
    (setf (aref (smf-tracks midi-file) 0) track)
    midi-file))

(defmethod ->midi ((section section) &key (filename nil)(offset 0.0)(repeat 1)(pad 2.0)(no-stripe nil))
  "Write section contents to Standard MIDI file."
  (let* ((midi-file (section->smf section :offset offset :repeat repeat :no-stripe no-stripe))
	 (output-filename (section-filename :section section :fname filename)))
    (write-smf midi-file output-filename :pad pad)
    midi-file))

(defun  get-section-part (section part-name)
  "Returns the named part from section.
It is a CYCO-ERROR if the part does not exists."
  (or (find-child section part-name)
      (cyco-error
       (sformat "Section ~A does not contain part named: ~A" (name section) part-name))))

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


(constant +NULL-SECTION+ (make-instance 'section
					:name 'NULL-SECTION
					:properties +SECTION-PROPERTIES+))
					
