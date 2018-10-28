;;;; CYCO3 src/composition/section
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

(flet ((typerror
	(parent child)
	(let ((msg1 "Section child must be a Part.")
	      (msg2 (sformat"~A ~A can not be a child of Section ~A"
			    (type-of child)(name child)(name parent))))
	  (cyco-type-error 'connect 'part child msg1 msg2))))
  
  (defmethod connect ((s section)(child t))
    (if (not (part-p child))
	(progn 
	  (typerror s child)
	  nil)
      (progn
	(call-next-method)
	(init-time-signature child)
	(put s :current-part child)
	child))))


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
:remarks - Optional remarks text."
  (if (not (project-p project))
      (cyco-value-error 'make-section project
			"project = nil"
			"No current project")
    (let ((s (make-instance 'section
			    :properties +section-properties+
			    :name (->symbol name)
			    :remarks remarks
			    :transient t)))
      (put s :cue-function cuefn)
      (put s :tempo tempo)
      (put s :unit unit)
      (put s :bars bars)
      (put s :beats beats)
      (put s :subbeats subbeats)
      (put s :current-part nil)
      (put s :groups '())
      (put s :transposable transposable)
      (put s :reversible reversible)
      (connect project s)
      (init-time-signature s)
      (put project :current-section s)
      (set-cyco-prompt)
      s)))

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
  "Same as make-section except binds the new section to a symbol named name."
  `(progn
     (banner2 (sformat "Section ~A" ',name))
     (if (not (symbolp ',name))
	 (cyco-type-error 'section 'symbol ',name
			  "Do not quote section name.")
       (let ((s (make-section ',name
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
	 (defparameter ,name s)
	 s))))

(defmethod groups ((s section))
  (property s :groups))

(defmethod has-group-p ((s section)(group-name symbol))
  (car (member group-name (groups s)
	       :test #'(lambda (a b)(eq a (name b))))))

(defmethod add-group ((s section)(grp group))
  (if (not (has-group-p s (name grp)))
      (put s :groups
	    (cons grp (property s :groups)))))

(defmethod mute-all ((sec section))
  (dolist (grp (property sec :groups))
    (mute grp :mute))
  (dolist (prt (children sec))
    (mute prt :mute)))

(defmethod unmute-all ((sec section))
  (dolist (grp (property sec :groups))
    (mute grp :unmute))
  (dolist (prt (children sec))
    (mute prt :unmute)))

(defmethod print-tree ((s section) &optional (depth 0))
  (call-next-method)
  (let ((gtab (spaces (* 4 (1+ depth)))))
    (dolist (grp (groups s))
      (format t "~AGroup ~16A State ~7A : " gtab (name grp)(mute-state grp))
      (dolist (prt (group-members grp))
	(format t "~A " (name prt)))
      (format t "~%"))))

(labels ((clone-groups
	  (src dst)
	  (dolist (grp (property src :groups))
	    (clone-group dst grp)))

	 ;; dst --> destination sections
	 ;; grp --> group to be cloned in src.
	 ;; ISSUE: member copy will not work if names differ between src
	 ;;        and dst sections.
	 ;;
	 (clone-group
	  (dst grp)
	  (let* ((member-names (let ((acc '()))
				 (dolist (prt (group-members grp))
				   (push (name prt) acc))
				 (reverse acc)))
		 (grp2 (make-group (name grp)
				   :member-names member-names
				   :section dst)))
	    (setf (mute-state grp2)(mute-state grp)))))

  (defmethod clone ((src section) &key new-name new-parent)
    (let* ((frmt (or new-name "~A"))
	   (name (->symbol (sformat frmt (name src))))
	   (parent (or new-parent (parent src)))
	   (dst (make-section name
			      :project parent
			      :cuefn (property src :cue-function)
			      :remarks (remarks src))))
      (put dst :chord-model (property src :chord-model))
      (copy-time-signature src dst)
      (dolist (c (children src))
	(let ((prt (clone c :new-name "~A" :new-parent dst)))
	  (put prt :muted (property c :muted))))
      (clone-groups src dst)
      dst)))

(defmethod transpose ((s section)(n t))
  (if (property s :transposable)
      (dolist (prt (children s))
	(transpose prt n)))
  s)

(defmethod invert ((s section)(pivot t))
  (if (property s :transposable)
      (dolist (prt (children s))
	(invert prt pivot)))
  s)

(defmethod retrograde ((s section))
  (if (property s :reversible)
      (dolist (c (children s))
	(retrograde c)))
  s)

(defmethod render-once ((sec section) &key (offset 0.0))
  (let* ((acc (list (cons offset
			  (midi-meta-marker (sformat "Start Section ~A" (name sec))))))
	 (period (phrase-duration sec))
	 (end-mask (+ offset period)))
    (dolist (prt (reverse (children sec)))
      (let* ((pperiod (phrase-duration prt))
	     (count (truncate (/ period pperiod)))
	     (shift (or (property prt :shift) 0.0))
	     (pevents (render-n prt count :offset (+ offset shift))))
	(dolist (evn pevents)
	  (let* ((time (car evn))
		 (msg (cdr evn)))
	    (if (or (< time end-mask)(not (midi-note-on-p msg)))
		(push (clone evn) acc))))))
    (sort-midi-events acc)))


(defmethod render-n ((sec section)(n integer) &key (offset 0.0))
  (let ((acc '())
	(period (phrase-duration sec))
	(template (render-once sec)))
    (dotimes (i n)
      (dolist (evn template)
	(let ((reltime (car evn))
	      (msg (cdr evn)))
	  (push (cons (+ offset (* i period) reltime) msg) acc))))
    (sort-midi-events acc)))

(defmethod dump-events ((s section) &key
			(range (cons 0.0 1e9))
			(filter #'false)
			(render nil))
  (dump-events (render-once s) :range range :filter filter :render render))
  
(defun section-filename (&key section fname)
  "Creates MIDI filename used to save section.  
Using the defaults with a project name 'foo and section name 'alpha
the filename is <user-home>/cyco-projects/foo/MIDI/alpha.mid

:section - sets an explicit section, defaults to the current section 
of *project*
:fname - sets an alternate filename.  If fname is an absolute position 
the it is used directly, otherwise the value of fname replaces the 
section name in the example above.   In all cases a .mid extension
is appended to the name if needed."
  (if (absolute-path-p fname)
      (return-from section-filename fname))
  (let* ((sec (or (and (section-p section) section)
		  (and (project-p *project*)
		       (property *project* :current-section))
		  (progn
		    (cyco-composition-error 'section-filename "No default project")
		    (return-from section-filename nil))))
	 (project (parent sec))
	 (pdir (property project :project-directory))
	 (pname (string-downcase (name project)))
	 (outdir (property project :output-directory))
	 (sname (string-downcase (or fname (name sec))))
	 (fqn (join-path-list (list pdir pname outdir sname) :as-file)))
    (append-filename-extension fqn ".mid")))
	       

(defmethod ->smf ((s section) &key
		  (filename nil)
		  (offset 0.0)
		  (repeat 1)
		  (pad 2.0))
  "Write section contents to Standard MIDI file."
  (let* ((fname (section-filename :section s :fname filename))
	 (events (render-n s repeat :offset offset))
	 (track (make-instance 'smf-track :events events))
	 (smf (smf :format 1 :track-count 1)))
    (setf (aref (smf-tracks smf) 0) track)
    (write-smf smf fname :pad pad)
    smf))
