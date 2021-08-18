;;;; CYCO pig plugin player.lisp
;;;;
;;;; Defines interface to Pigiron MIDIPlayer operator.
;;;; The global variable pig:*midi-player* holds name for the
;;;; MIDIPlayer operator, default name is 'player'
;;;;

(in-package :pig)

(global *midi-player* "player")

(defun player-ping ()
  "(pig:player-ping) sends ping command to Pigiron MIDIPlayer named by pig:*midi-player*"
  (osc-send "exec" (sformat "op ~A, ping" *midi-player*)))
  

(defun stop ()
  "(pig:stop) sends stop message to Pigiron player named by pig:*midi-player*"
  (osc-send "exec" (sformat "op ~A, stop" *midi-player*)))

;; ISSUE: Not portability 
(defun is-absolute-filename-p (filename)
  (and (plusp (length filename))
       (or (eq (char filename 0) #\/)
	   (eq (char filename 0) #\~))))

;; If name is relative to current project, do not add .mid extension
;;
(defun resolve-midi-filename (&optional name)
  (cond ((is-absolute-filename-p name) name)
	((and *project* name)
	 (join-path (property *project* :project-directory)
		    (string-downcase (->string (name *project*)))
		    (property *project* :output-directory)
		    (sformat "~A.mid" name)
		    :as-file))
	(*project*
	 (let ((s (property *project* :current-section)))
	   (if s (join-path  (property *project* :project-directory)
			     (string-downcase (->string (name *project*)))
			     (property *project* :output-directory)
			     (string-downcase (sformat "~A.mid" (name s)))
			     :as-file))))
	(t nil)))
	
(defun load-smf (name)
  "(pig:load-smf name) Sends load command to Pigiron player named by pig:*midi-player*
name argument should be an absolute filename for a MIDI file."
  (osc-send "exec" (sformat "op ~A, load, ~A" *midi-player* name))
  (sleep 0.1))
  

(defun play (&optional name)
  "(pig:play &optional name) Sends play command to Pigiron player named by pig:*midi-player*

If optional name is specified it instructs the player first to load a MIDI file.  
Name may take the following forms:
   absolute - load name as stated.
   non-absolute - load the file relative to the current project, do not include
   filename extension."
  (if name
      (let ((filename (resolve-midi-filename name)))
	(if filename
	    (load-smf filename)
	  (cyco-error (sformat "PIG:PLAY can not load filename ~A" name)))))
  (osc-send "exec" (sformat "op ~A, play" *midi-player*)))
  

