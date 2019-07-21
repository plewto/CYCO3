;;; CYCO
;;;; 

(constant +project-properties+
	  (append +time-signature-properties+
		  '(:title 
		    :catalog-number
		    :cue-function
		    :shuffle-function
		    :project-directory
		    :main-file
		    :output-directory
		    :chord-model
		    :section-order
		    :current-section)))


(defclass project (time-signature) ()
  (:documentation
   "A Project is the top-level composition object and corresponds to a 
single piece or song.  Projects define the default time-signature, 
cueing-function, chord-model and maintain a list of Sections.

Sections are equivalent to major divisions of the composition, such as 
a movement, verse, chorus etc.  Projects also maintain pointers to 
to a project directory.  All files for a specific project are stored 
in a single directory. 

When a project is created it is automatically bound to the global
symbol *project*    

Most project level functions use *project* by default."))


(defmethod project-p ((p project)) t)

(defun format-project-main-filename (project-name &optional (format *project-main-filename-format*))
  (sformat format (string-downcase (->string project-name))))


(let ((docstring
        "Creates new instance of Project.
name  - Symbol, the project's name.
        A lowercase version of the name is used to locate the
        project's directories relative to project-directory.
:title - Composition title, defaults to name.
:cuefn - Default cueing function, default #'bar
:shuffle - Default shuffle function, defaults to #'no-shuffle
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
abbreviations lp and lpf."))

  (defun make-project (name &key
			    title
			    (catalog-number "")
			    (project-directory *default-project-directory*)
			    main-file
			    (output-directory *default-project-output-directory*)
			    (cuefn #'bar)
			    (shuffle #'no-shuffle)
			    (tempo 60.0)
			    unit
			    (bars 4)
			    (beats 4)
			    (subbeats 4)
			    (remarks "")
			    (make-current t))
    docstring
    (banner1 (sformat "Project ~A" name))
    (save-persistent-project-name name)
    (let ((project (make-instance 'project
				  :name (->symbol name)
				  :remarks (->string remarks)
				  :properties +project-properties+)))
      (put project :title title)
      (put project :catalog-number catalog-number)
      (put project :project-directory project-directory)
      (put project :main-file (or main-file (format-project-main-filename name)))
      (put project :output-directory output-directory)
      (put project :chord-model *chord-table*)
      (put project :cue-function cuefn)
      (put project :shuffle-function shuffle)
      (put project :tempo (float tempo))
      (put project :unit (or unit 'q))
      (put project :bars (truncate bars))
      (put project :beats (truncate beats))
      (put project :subbeats (truncate subbeats))
      (put project :section-order '())
      (put project :current-section nil)
      (init-time-signature project)
      (if make-current
	  (progn
	    (setf *project* project)
	    (set-cyco-prompt)))
      project))) 


(defmacro project (name &key
			title
			(catalog-number "")
			(project-directory *default-project-directory*)
			main-file
			(output-directory *default-project-output-directory*)
			(cuefn #'bar)
			(shuffle #'no-shuffle)
			(tempo 60.0)
			unit
			(bars 4)
			(beats 4)
			(subbeats 4)
			(remarks ""))
  "Identical to make-project except that the new project is also bound to 
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
		     :shuffle ,shuffle
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

;; NOTE 1: current-section property is not preserved.
;; NOTE 2: global *project* is not altered.
;;
(defmethod clone ((source-project project) &key new-name new-parent)
  (dismiss new-parent)
  (let* ((frmt (or new-name "~A"))
	 (cloned-project (make-project (->symbol (sformat frmt (name source-project)))
			    :title (property source-project :title)
			    :catalog-number (property source-project :catalog-number)
			    :project-directory (property source-project :project-directory)
			    :main-file (property source-project :main-file)
			    :output-directory (property source-project :output-directory)
			    :cuefn (property source-project :cue-function)
			    :tempo (property source-project :tempo)
			    :unit (property source-project :unit)
			    :bars (property source-project :bars)
			    :beats (property source-project :beats)
			    :subbeats (property source-project :subbeats)
			    :remarks (remarks source-project)
			    :make-current nil)))
    (init-time-signature cloned-project)
    (put cloned-project :chord-model (property source-project :chord-model))
    (dolist (c (children source-project))
      (clone c :new-name "~A" :new-parent cloned-project))
    (put cloned-project :section-order (clone (property source-project :section-order)))
    cloned-project))

(defun prune-project (section-name &key (project *project*))
  "prune-project removes named section from project."
  (dolist (child (children project))
    (if (eq (name child) section-name)
	(disconnect child))))
  
