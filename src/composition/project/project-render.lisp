;;;; CYCO composition/project project-render.lisp
;;;;
;;;; Methods for converting project to MIDI file.
;;;;

(in-package :cyco)


;; *NOTE about render-project
;; As part of rendering a project all of the sections are cloned.
;; If a section happens to be the 'current-section' then cloning it replaces
;; the current-section with the cloned version.   Normally this is desired.
;; However it is undesirable in the context of rendering a project.
;; The purpose of temp-current-section is to restore the current-section
;; after rendering the project.
;;
(defun render-project (&optional (project *project*))
  "Converts project to a MIDI event list."
  (banner1 (sformat "Rendering Project: ~A" (name project)))
  (let ((acc '())
	(time 0.0)
	(temp-current-section (property project :current-section))) ;; *see note above
    (dolist (smode (property project :section-order))
      (let* ((section-name (section-render-mode-section-name smode))
	     (count (section-render-mode-count smode))
	     (xpose (section-render-mode-transpose smode))
	     (shift (section-render-mode-shift smode))
	     (invert-pivot (section-render-mode-invert smode))
	     (working-section (clone (find-child project section-name) :rename-parts nil :bind nil)))
    	(if (section-p working-section)
    	    (let ((period (phrase-duration working-section)))
    	      (if (not (zerop xpose))(transpose working-section xpose))
    	      (if invert-pivot (invert working-section invert-pivot))
    	      (setf acc (append acc (render-n working-section count :offset (+ shift time))))
    	      (setf time (+ time (* count period)))
    	      (disconnect working-section))
    	  (cyco-warning
    	   (sformat t "Section ~A does not exists" section-name)))))
    (put project :current-section temp-current-section)
    (sort-midi-events acc)))

(defun project->midi (&key (project *project*)
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
    (put project :midi-filename fname)
    (write-smf smf fname)
    smf))


(defun partition-project (&key (project *project*)(unmute-all t))
  "Render project into individual channels.
A MIDI file is created for each non-empty channel for each section.
If unmute-all is true, all parts are unmuted."
  (dolist (sec (children project))
    (format t "Section ~A~%" (name sec))
    (partition sec :unmute-all unmute-all)))
    
