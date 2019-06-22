;;;; CYCO
;;;; 

(constant +project-properties+
	  (append +time-signature-properties+
		  '(:title :catalog-number
			   :cue-function
			   :project-directory
			   :main-file
			   :output-directory
			   :chord-model
			   :section-order
			   :current-section)))

(defstruct seq-mode
  section-name
  shift ;; initiasl time offset what units?
  count ;; <= 0 --> skip
  transpose
  invert
  retrograde)
  
(defmethod clone ((src seq-mode) &key new-name new-parent)
  (dismiss new-name new-parent)
  (make-seq-mode
   :section-name (seq-mode-section-name src)
   :shift (seq-mode-shift src)
   :count (seq-mode-count src)
   :retrograde (seq-mode-retrograde src)
   :transpose (seq-mode-transpose src)
   :invert (seq-mode-invert src)))
   

(defclass project (time-signature) ()
  (:documentation
   "A Project is the top-level composition object and corresponds to a 
single piece or song.  Projects define the default time-signature, 
cueing-function, and chord-model and maintain a list of Sections.
Sections are equivalent to major divisions of the composition,such as 
a movement, verse, chorus etc.  Projects also maintain pointers to 
to a project directory.  All files for a specific project are stored 
in a single directory. 

When a project is created it is automatically bound to the global symbol 
*project*, which serve as the default for most composition functions.

It is possible to have more then one active project in memory at a time
but only one will be bound to *project* as the default."))

(defmethod project-p ((p project)) t)

(global *persistent-project-name-namestring* "current-project-name")

(defun save-persistent-project-name (name)
  "Saves the current project name to a file in the config folder.
This file may be used to reload the the project the next time CYCO is
used."
  (let* ((filename (join-path *config-directory*
			      *persistent-project-name-namestring*
			      :as-file))
	 (stream (open filename
		       :direction :output
		       :if-does-not-exist :create
		       :if-exists :overwrite)))
    (format stream (->string name))
    (close stream)))


(defun read-persistent-project-name ()
"Returns the project name, if any, saved by 
save-persistent-project-name.  Returns nil if there is 
no saved project."
  (let* ((filename (join-path *config-directory*
			      *persistent-project-name-namestring*
			      :as-file))
	 (stream (if (probe-file filename)
		     (open filename
			   :direction :input))))
    (if stream
	(prog1
	    (read-line stream)
	  (close stream))
      nil))) 

(defun format-project-main-filename (project-name &optional (format *project-main-filename-format*))
  (sformat format (string-downcase (->string project-name))))


(defun make-project (name &key
			  title
			  (catalog-number "")
			  (project-directory *default-project-directory*)
			  ;;(main-file *project-main-filename-format*)
			  main-file
			  (output-directory *default-project-output-directory*)
			  (cuefn #'bar)
			  (tempo 60.0)
			  unit
			  (bars 4)
			  (beats 4)
			  (subbeats 4)
			  (remarks "")
			  (make-current t))
  "Creates new instance of Project.
name  - Symbol, the project's name.
        A lowercase version of the name is used to locate the
        project's directories relative to project-directory.
:title - Composition title, defaults to name.
:cuefn - Default cueing function, default #'bar
:tempo - Default tempo in BPM, default 60.
:unit  - Default time-signature unit, default 'Q
:bars  - Default number of bars per phrase, default 4.
:beats - Default number of beats per bar, default 4.
:subbeats - Default number of sub-beats per beat, default 16
:remarks  - Optional remarks text.
:catalog-number - Optional catalog number.
:make-current   - boolean, if true the new project is made the default 
                  project by binding it to *project*.
:project-directory - Sets the top level project directory where this 
                     projects files are stored.  Defaults to *default-project-directory* 
:main-file -  Sets the lisp file name for the main project file.
              This file is responsible for loading all other project files.
              Defaults to name-main.lisp
:output-directory - Output directory where rendered MIDI files are saved.
                    Defaults to *default-project-output-directory*

The location of the project's files is determined by a combination 
of it's name and the values of project-directory, main-file and 
output-directory.

Using the defaults of 
    *default-project-directory*  --> <user-home>/cyco-projects
    *default-project-name*       --> main.lisp
    *default-project-output-directory* --> MIDI

The files for a project named 'foo are located at
    <user-home/cyco-projects/foo/

Note it is not possible to include spaces in a project's name,
and the project's folder name is always lowercase.

The main project file will be
    <user-home>/cyco-projects/foo/main.lisp

And MIDI files will be placed in folder
    <user-home>/cyco-projects/foo/MIDI/

All other project related files should be placed in 
the cyco-projects/foo/ directory.  


There is no prescribed standard for naming other project files 
but having a dedicated file for each section makes sense.

See the functions load-project, load-project-file and the
abbreviations lp and lpf."
  (banner1 (sformat "Project ~A" name))
  (save-persistent-project-name name)
  (let ((proj (make-instance 'project
			     :name (->symbol name)
			     :remarks (->string remarks)
			     :properties +project-properties+)))
    (put proj :title title)
    (put proj :catalog-number catalog-number)
    (put proj :project-directory project-directory)
    (put proj :main-file (or main-file (format-project-main-filename name)))
    (put proj :output-directory output-directory)
    (put proj :chord-model *chord-table*)
    (put proj :cue-function cuefn)
    (put proj :tempo (float tempo))
    (put proj :unit (or unit 'q))
    (put proj :bars (truncate bars))
    (put proj :beats (truncate beats))
    (put proj :subbeats (truncate subbeats))
    (put proj :section-order '())
    (put proj :current-section nil)
    (init-time-signature proj)
    (if make-current
	(progn
	  (setf *project* proj)
	  (set-cyco-prompt)))
    proj))


(defmacro project (name &key
			  title
			  (catalog-number "")
			  (project-directory *default-project-directory*)
			  main-file
			  (output-directory *default-project-output-directory*)
			  (cuefn #'bar)
			  (tempo 60.0)
			  unit
			  (bars 4)
			  (beats 4)
			  (subbeats 4)
			  (remarks ""))
  "Identical to make-project except that the new project is bound to 
a symbol named name."
  `(if (not (symbolp ',name))
       (cyco-type-error 'project 'symbol ',name
			"Do not quote project name.")
     (progn 
       (make-project ',name
		     :title ,title
		     :catalog-number ,catalog-number
		     :project-directory ,project-directory
		     :main-file ,main-file
		     :output-directory ,output-directory
		     :cuefn ,cuefn
		     :tempo ,tempo
		     :unit ,unit
		     :bars ,bars
		     :beats ,beats
		     :subbeats ,subbeats
		     :remarks ,remarks)
       (defparameter ,name *project*)
       *project*)))

(defmethod connect ((project project)(section cyco-node))
  (if (not (section-p section))
      (cyco-composition-error 'project.connect
			      (sformat "Section ~A does not exists" section))
    (progn
      (call-next-method)
      (init-time-signature section)
      (put project :current-section section))))

(defun seq-mode (name &key (shift 0.0)(x 1)(trans 0)(invert nil)(retro nil))
  (make-seq-mode :section-name name
		 :shift (float shift)
		 :count x
		 :transpose trans
		 :invert invert
		 :retrograde retro))


;; ISSUE: Include specific seq-order method documentation.
(defmethod seq-order ((s seq-mode) &key (project *project*))
  (let ((sname (seq-mode-section-name s)))
    (if (find-child project sname)
	(put project :section-order
	      (reverse (cons s (reverse (property project :section-order)))))
      (cyco-composition-error 'seq-order
			      (sformat "Section ~A does not exists" sname)) )))

(defmethod seq-order ((section-name symbol) &key (project *project*))
  (if (find-child project section-name)
      (let ((smode (make-seq-mode :section-name section-name
				  :count 1
				  :transpose 0
				  :invert nil)))
	(seq-order smode :project *project*))
    (cyco-composition-error 'seq-order
			    (sformat "Section ~A does not exists" section-name))))

(flet ((type-error (spec expected offending)
		   (cyco-type-error 'project.seq-order expected offending
				    (sformat "Invalid seq-order specification: ~A" spec))))

  ;; Takes list of section sequence order.
  ;; Section specification may be single symbol for section-name
  ;; or list (section-name :x 4 :trans 0 :invert kn :shift time :retro bool)
  ;; **BUGGY** causes project-reder to fail???    
  (defmethod seq-order ((seqlist list) &key (project *project*))
    "Establish Section order.
seqlist is a nested list of form 

     ((section-name :x n  :trans tx :invert kn :retro b :shift time)
       ..........................................)

All elements are optional with the exception of the section-name.   If there 
are no modifier terms then the section name may be used by itself without 
placing it in a list.

The modifiers:
  :x n - repeat section n time.
  :trans tx - trans by tx half-steps
  :invert kn - invert keys around pivot keynumber kn.
  :retrograde flag - if true reverse section.
  :shift time - move start of section by time seconds.

Additional modifiers may be added later."


    (dolist (q seqlist)
      (let* ((spec (->list q))
	     (section-name (car spec))
	     (count (or (second (member :x spec)) 1))
	     (shift (or (second (member :shift spec)) 0.0))
	     (retro (or (second (member :retro spec)) nil))
	     (transpose (or (second (member :trans spec)) 0))
	     (invert (second (member :invert spec))))
	(if (not (symbolp section-name))(type-error q 'symbol section-name))
	(if (not (integerp count))(type-error q 'integer count))
	(if (not (integerp transpose))(type-error q 'integer transpose))
	(if (not (or (null invert)(keynumber-p invert)))(type-error q 'keynumber invert))
	(seq-order (make-seq-mode :section-name section-name
				  :count count
				  :retrograde retro
				  :shift (float shift)
				  :transpose transpose
				  :invert (if invert (keynumber invert) nil))
		   :project project)))))
	

(defun clear-seq-order (&optional (project *project*))
  "Removes all section-order information established by seq-order."
  (put project :section-order '()))


(let ((current-project-main-file nil)
      (current-filename nil)
      (frmt "Loading project file ~A~%"))

  (defun default-project-name (name)
    (string-downcase
     (->string (or name
		   (and (project-p *project*)(name *project*))
		   (read-persistent-project-name)))))
  
  (defun load-project (name &key
			    (project-directory *default-project-directory*)
			    main-file)
    "Loads the main project file, which should then load the remaining files.

name should be a symbol matching the project's directory.

The optional project-directory and main-file arguments may be used to 
specify a non-standard location.

See LP as a more convenient method of loading a project."

    (let ((pname (default-project-name name)))
      (if pname
	  (let ((fqn (join-path project-directory pname
				(or main-file
				    (format-project-main-filename pname))
				:as-file)))
	    (setf current-project-main-file fqn)
	    (format t frmt fqn)
	    (load fqn))
	(cyco-composition-error
	 'load-project
	 "Either there is no default project name,"
	 (sformat "or name argument is invalid: ~A ~A" (type-of name) name)))))

  (defun lp (&optional name)
    "LP is a short-cut for load-project with useful defaults.
The first time it is used the optional name argument should be specified as in 
load-project.  Thereafter subsequent calls to LP will default to reloading the 
current project.  

Whenever a new project is loaded it's name is saved to a file in the config 
folder.  The next time CYCO is started the previous project may be reloaded simply 
by calling LP without an argument."
    (load-project name))
  
  (defun load-project-file (name)
    "load-project-file loads a lisp file relative to the project's directory.
The name argument is a symbol which is converted to a lowercase string.
See LPF for a convenient shortcut."
    (let ((sname (cond ((symbolp name)
			(string-downcase (->string name)))
		       ((stringp name)
			name)
		       (t (cyco-type-error 'load-project-file '(string symbol) name)))))
      (if sname
	  (let* ((ppath (path-parent current-project-main-file))
		 (fqn (join-path ppath sname :as-file)))
	    (setf current-filename fqn)
	    (format t frmt fqn)
	    (load fqn)))))
  
  (defun lpf (&optional name)
    "LPF is a short-cut for load-project-file.  When used without an argument
it reloads the most recent project-file.  It is useful for interactively 
reloading a file while it is under development."
    (if name
	(load-project-file name)
      (if current-filename
	  (progn 
	    (format t frmt current-filename)
	    (load current-filename))
	(cyco-composition-error 'lpf name
				"Section does not exists")))) )
  

;; NOTE 1: current-section property is not preserved.
;; NOTE 2: global *project* is not altered.
;;
(defmethod clone ((src project) &key new-name new-parent)
  (dismiss new-parent)
  (let* ((frmt (or new-name "~A"))
	 (dst (make-project (->symbol (sformat frmt (name src)))
			    :title (property src :title)
			    :catalog-number (property src :catalog-number)
			    :project-directory (property src :project-directory)
			    :main-file (property src :main-file)
			    :output-directory (property src :output-directory)
			    :cuefn (property src :cue-function)
			    :tempo (property src :tempo)
			    :unit (property src :unit)
			    :bars (property src :bars)
			    :beats (property src :beats)
			    :subbeats (property src :subbeats)
			    :remarks (remarks src)
			    :make-current nil)))
    (init-time-signature dst)
    (put dst :chord-model (property src :chord-model))
    (dolist (c (children src))
      (clone c :new-name "~A" :new-parent dst))
    (put dst :section-order (clone (property src :section-order)))
    dst))

(defun render-project (&optional (project *project*))
  "Converts project to a MIDI event list."
  (banner1 (sformat "Rendering Project: ~A" (name project)))
  (let ((acc '())
	(time 0.0))
    (dolist (smode (property project :section-order))
      (let* ((section-name (seq-mode-section-name smode))
	     (count (seq-mode-count smode))
	     (xpose (seq-mode-transpose smode))
	     (shift (seq-mode-shift smode))
	     (retro (seq-mode-retrograde smode))
	     (invert-pivot (seq-mode-invert smode))
	     (section (clone (find-child project section-name))))
	(if (section-p section)
	    (let ((period (phrase-duration section)))
	      (if (not (zerop xpose))(transpose section xpose))
	      (if invert-pivot (invert section invert-pivot))
	      (if retro (retrograde section))
	      (setf acc (append acc (render-n section count :offset (+ shift time))))
	      (setf time (+ time (* count period)))
	      (disconnect section))
	  (cyco-warning
	   (sformat t "Section ~A does not exists" section-name)))))
    (sort-midi-events acc)))

(defun project->smf (&key (project *project*)
			  (filename nil))
  "Saves project to a midi file in the projects output directory.
The filename defaults to the projects name.  For project foo the default
file is  <user-home>/cyco-projects/foo/MIDI/foo.mid"
  (let* ((track (make-instance 'smf-track :events (render-project project)))
	 (smf (let ((mf (smf :format 1 :track-count 1)))
		(setf (aref (smf-tracks mf) 0) track)
		mf))
	 (fname (append-filename-extension
		 (if (absolute-path-p filename)
		     filename
		   (join-path-list (list (property project :project-directory)
					 (string-downcase (name project))
					 (property project :output-directory)
					 (property project :main-file))
				   :as-file))
		 ".mid")))
    (write-smf smf fname)
    smf))


(flet ((format-name (sym)
		    (string-downcase (->string sym))))

  (defun create-project-framework (project-name &rest extras)
    "Creates project directories and specified (empty) files.
project-name - quoted symbol

Each additional argument must be a quoted symbol and is used to create
a matching project-file with the same name.   The main project file
is automatically created.   Existing files are not overwritten."
    (let* ((pname (format-name project-name))
	   (pdir (join-path *default-project-directory* pname))
	   (outdir (join-path pdir *default-project-output-directory*)))
      (format t "Creating project ~A directory framework~%" project-name)
      (dolist (d (list pdir outdir))
	(format t "Ensuring directory exist: ~A~%" d)
	(ensure-directories-exist d))
      (dolist (fn (cons (sformat *project-main-filename-format* project-name)
			extras))
	(let ((fqn (append-filename-extension
		    (join-path pdir (format-name fn) :as-file)
		    ".lisp")))
	  (if (not (probe-file fqn))
	      (progn
		(format t "Creating project file: ~A~%" fqn)
		(let ((stream (open fqn
				    :direction :output
				    :if-does-not-exist :create)))
		  (format stream ";;;; CYCO Project ~A  File ~A.lisp~%" pname (format-name fn))
		  (format stream ";;;;~%~%")
		  (format stream (sformat "(version ~A)~%~%" (car +cyco-version+)))
		  (format stream (sformat "(project ~A)~%" project-name))
		  (close stream)))
  	    (format t "Project file already exists: ~A~%" fqn)))))))




