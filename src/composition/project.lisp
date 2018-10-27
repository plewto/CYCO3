;;;; CYCO3 src/composition/project
;;;;

;; (defgeneric project-p (obj))
;; (defgeneric section-p (obj))
;; (defgeneric part-p (obj))
;; (defgeneric group-p (obj))
(defgeneric seq-order (sections &key project))
(defgeneric groups (section))
(defgeneric has-group-p (section group-name))
(defgeneric add-group (section group))
(defgeneric mute (obj &optional state))  ;; state one of :mute :unmute :solo or nil
(defgeneric solo (obj))
(defgeneric unmute (obj))
(defgeneric muted-p (obj))
(defgeneric mute-all (obj))
(defgeneric unmute-all (obj))

(global *default-project-main-file* "main")

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
  count ;; <= 0 --> skip
  transpose
  invert)

(defmethod clone ((src seq-mode) &key new-name new-parent)
  (dismiss new-name new-parent)
  (make-seq-mode
   :section-name (seq-mode-section-name src)
   :count (seq-mode-count src)
   :transpose (seq-mode-transpose src)
   :invert (seq-mode-invert src)))
   

(defclass project (time-signature) ())

(defmethod project-p ((p project)) t)


(global *persistent--project-name-namestring* "current-project-name")

(defun save-persistent-project-name (name)
  (let* ((filename (join-path *cyco-config-directory*
			      *persistent--project-name-namestring*
			      :as-file))
	 (stream (open filename
		       :direction :output
		       :if-does-not-exist :create
		       :if-exists :overwrite)))
    (format stream (->string name))
    (close stream)))


(defun read-persistent-project-name ()
  (let* ((filename (join-path *cyco-config-directory*
			      *persistent--project-name-namestring*
			      :as-file))
	 (stream (if (probe-file filename)
		     (open filename
			   :direction :input))))
    (if stream
	(prog1
	    (read-line stream)
	  (close stream))
      nil))) 

(defun make-project (name &key
			  title
			  (catalog-number "")
			  (project-directory *default-project-directory*)
			  (main-file *default-project-main-file*)
			  (output-directory *default-project-output-directory*)
			  (cuefn #'bar)
			  (tempo 60.0)
			  unit
			  (bars 4)
			  (beats 4)
			  (subbeats 4)
			  (remarks "")
			  (make-current t))
  (banner1 (sformat "Project ~A" name))
  (save-persistent-project-name name)
  (let ((proj (make-instance 'project
			     :name (->symbol name)
			     :remarks (->string remarks)
			     :properties +project-properties+)))
    (put proj :title title)
    (put proj :catalog-number catalog-number)
    (put proj :project-directory project-directory)
    (put proj :main-file main-file)
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
			  (main-file *default-project-main-file*)
			  (output-directory *default-project-output-directory*)
			  (cuefn #'bar)
			  (tempo 60.0)
			  unit
			  (bars 4)
			  (beats 4)
			  (subbeats 4)
			  (remarks ""))
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
			    (sformat "Sectioon ~A does not exists" section-name))))

(flet ((type-error (spec expected offending)
		   (cyco-type-error 'project.seq-order expected offending
				    (sformat "Invalid seq-order specification: ~A" spec))))
  ;; Takes list of section sequence order.
  ;; Section specification may be single symbol for section-name
  ;; or list (section-name :x 4 :transpose 0 :invert kn)
  ;;     
  (defmethod seq-order ((seqlist list) &key (project *project*))
    (dolist (q seqlist)
      (let* ((spec (->list q))
	     (section-name (car spec))
	     (count (or (second (member :x spec)) 1))
	     (transpose (or (second (member :transpose spec)) 0))
	     (invert (second (member :invert spec))))
	(if (not (symbolp section-name))(type-error q 'symbol section-name))
	(if (not (integerp count))(type-error q 'integer count))
	(if (not (integerp transpose))(type-error q 'integer transpose))
	(if (not (or (null invert)(keynumber-p invert)))(type-error q 'keynumber invert))
	(seq-order (make-seq-mode :section-name section-name
				  :count count
				  :transpose transpose
				  :invert (if invert (keynumber invert) nil))
		   :project project)))))
	

(defun clear-seq-order (&optional (project *project*))
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
			    (main-file *default-project-main-file*))
    (let ((pname (default-project-name name)))
      (if pname
	  (let ((fqn (join-path project-directory pname main-file :as-file)))
	    (setf current-project-main-file fqn)
	    (format t frmt fqn)
	    (load fqn))
	(cyco-composition-error
	 'load-project
	 "Either there is no default project name,"
	 (sformat "or name argument is invalid: ~A ~A" (type-of name) name)))))

  ;; (defun lp (&optional name)
  ;;   (if name
  ;; 	(load-project name)
  ;;     (if current-project-main-file
  ;; 	  (progn 
  ;; 	    (format t frmt current-project-main-file)
  ;; 	    (load current-project-main-file))
  ;; 	(cyco-composition-error 'lp
  ;; 				"No default project"
  ;; 				"Try loading or creating project first."))))

  (defun lp (&optional name)
    (load-project name))
  
  (defun load-project-file (name)
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
  (banner1 (sformat "Rendering Project: ~A" (name project)))
  (let ((acc '())
	(time 0.0))
    (dolist (smode (property project :section-order))
      (let* ((section-name (seq-mode-section-name smode))
	     (count (seq-mode-count smode))
	     (xpose (seq-mode-transpose smode))
	     (invert-pivot (seq-mode-invert smode))
	     (section (clone (find-child project section-name))))
	(if (section-p section)
	    (let ((period (phrase-duration section)))
	      (if (not (zerop xpose))(transpose section xpose))
	      (if invert-pivot (invert section invert-pivot))
	      (setf acc (append acc (render-n section count :offset time)))
	      (setf time (+ time (* count period)))
	      (disconnect section))
	  (cyco-warning
	   (sformat t "Section ~A does not exists" section-name)))))
    (sort-midi-events acc)))

(defun project->smf (&key (project *project*)
			  (filename nil))
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
		
		 

