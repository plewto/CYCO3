;;;; CYCO composition/project project-render.lisp
;;;;
;;;; Methods for converting project to MIDI file.
;;;;

(in-package :cyco)

(defun render-project (&optional (project *project*))
  "Converts project to a MIDI event list."
  (banner1 (sformat "Rendering Project: ~A" (name project)))
  (let ((acc '())
	(time 0.0))
    (dolist (smode (property project :section-order))
      (let* ((section-name (section-render-mode-section-name smode))
	     (count (section-render-mode-count smode))
	     (xpose (section-render-mode-transpose smode))
	     (shift (section-render-mode-shift smode))
	     (retro (section-render-mode-retrograde smode))
	     (invert-pivot (section-render-mode-invert smode))
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
    (write-smf smf fname)
    smf))
