;;;; CYCO composition/project project.lisp
;;;; 
;;;; Defines the top-level project class.
;;;;

(in-package :cyco)

(constant +project-properties+
	  (append +time-signature-properties+
		  '(:title 
		    :catalog-number
		    :cue-function
		    :shuffle-function
		    :project-directory
		    :main-file
		    :output-directory
		    :midi-filename  ;; filename of most recently rendered MIDI file
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

(param *make-project-hook* #'(lambda (_)(declare (ignore _))))
			       


(let ((docstring
        "Creates new instance of Project.
name  - Symbol, the project's name.
        A lowercase version of the name is used to locate the
        project's directories relative to project-directory.
:title - Composition title, defaults to name.
:cuefn - Default cueing function, default *default-cue-function*
:shuffle - Default shuffle function, defaults to #'no-shuffle
:tempo - Defaults to *root-time-signature* tempo.
:unit  - Defaults to *root-time-signature* unit.
:bars  -Defaults to *root-time-signature* bars.
:beats - Defaults to *root-time-signature* beats.
:subbeats - Defaults to *root-time-signature* subbeats.
:remarks  - Optional remarks text.
:catalog-number - Optional catalog number.
:make-current   - boolean, if true the new project is made the default 
                  project by binding it to *project*.
:project-directory - Sets the top level project directory where this 
                     projects files are stored.  Defaults to *projects-root* 
:main-file -  Sets the lisp file name for the main project file.
              This file is responsible for loading all other project files.
              Defaults to name-main.lisp
:output-directory - Output directory where rendered MIDI files are saved.
                    Defaults to *default-project-output-directory*

The location of the project's files is determined by a combination 
of its name and the values of project-directory, main-file and 
output-directory.

Using the defaults of 
    *projects-root*  --> <user-home>/cyco-projects
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
			    (project-directory *projects-root*)
			    main-file
			    (output-directory *default-project-output-directory*)
			    (cuefn *default-cue-function*)
			    (shuffle #'no-shuffle)
			    (tempo (tempo *root-time-signature*))
			    (unit (unit *root-time-signature*))
			    (bars (bars *root-time-signature*))
			    (beats (beats *root-time-signature*))
			    (subbeats (subbeats *root-time-signature*))
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
      (put project :midi-filename nil)
      (put project :current-section nil)
      (init-time-signature project)
      (if make-current
	  (progn
	    (setf *project* project)))
      (funcall *make-project-hook* (list (list :name name)
					 (list :project-directory project-directory)
					 (list :output-directory output-directory)))
      project))) 


(defmacro project (name &key
			title
			(catalog-number "")
			(project-directory *projects-root*)
			main-file
			(output-directory *default-project-output-directory*)
			(cuefn *default-cue-function*)
			(shuffle #'no-shuffle)
			tempo
			unit
			bars
			beats
			subbeats
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
		     :tempo (or ,tempo (tempo *root-time-signature*))
		     :unit (or ,unit (unit *root-time-signature*))
		     :bars (or ,bars (bars *root-time-signature*))
		     :beats (or ,beats (beats *root-time-signature*))
		     :subbeats (or ,subbeats  (subbeats  *root-time-signature*))
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
(defmethod clone ((mother project) &key new-name new-parent)
  (declare (ignore new-name new-parent))
  (cyco-error "Cloning of project not supported."
	      (sformat "Can not clone project ~A" (name mother)))
  mother)

(defun prune-project (section-name &key (project *project*))
  "prune-project removes named section from project."
  (dolist (child (children project))
    (if (eq (name child) section-name)
	(disconnect child))))

(defun project-location (&optional (prj *project*))
  (if (project-p prj)
      (join-path (property prj :project-directory)
		 (string-downcase (name prj)))))

(constant +NULL-PROJECT+ (make-instance 'project
					:name 'null-project
					:properties +PROJECT-PROPERTIES+))

(push-special-directory "@project" "Current project" #'project-location)
(push-special-directory "@out" "Current project output"
			#'(lambda ()
			    (if (project-p *project*)
				(join-path (project-location)
					   (property *project* :output-directory))
			      (user-home))))

(put +NULL-PROJECT+ :tempo 60)
(put +NULL-PROJECT+ :bars 1)
(put +null-project+ :unit 'q)

					
