;;;; pig plugin
;;;;
;;;; CYCO interface to Pigiron
;;;; Pigiron is a MIDI routing utility with integrated MIDI file player,
;;;; available at github.com/plewto/Pigiron
;;;;

(cyco-warning "The PIG plugin is broken after OS update." "See BUG 0023" "")

;; (load "~/quicklisp/setup")
(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :osc)
  (ql:quickload :usocket)
  (ql:quickload :uiop))

(global *pig-sleep-time* 1.0)
(global *pig-prefix* "/pig")
(global *pig-host* #(127 0 0 1))
(global *pig-port* 8020)
(global *pig-output-channels* '(1))
(global *pig-midi-filename* "")
(global *pig-player-op* "player")
(global *pig-output-op* "out")
(global *pig-distributor-op* "dist")

(labels ((format-address (command)
			 (sformat "~A/~A" *pig-prefix* command))
	 
	 (bool->str (flag) (if flag "true" "false"))

	 ;; ISSUE: Not portability 
	 (is-absolute-filename-p (filename)
				 (and (plusp (length filename))
				      (or (eq (char filename 0) #\/)
					  (eq (char filename 0) #\~))))
	 
	 ;; If name is relative to current project, do not add .mid extension
	 ;;
	 (resolve-midi-filename (&optional name)
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
	 
	 (midi->pig (&rest bytes)
		    (let ((acc (sformat "midi, ~A, " *pig-output-op*)))
		      (dolist (b bytes)
			(setf acc (sformat "~A ~A," acc b)))
		      (setf acc (subseq acc 0 (1- (length acc))))
		      (pig-send "exec" acc)))
	 
	 (->status (channel stat)
		   (let ((ci (logand (1- channel) #x0F)))
		     (logior stat ci))) )
	 
  (defun pig-send (command &optional (data 0))
    (let* ((address (format-address command))
	   (socket (usocket:socket-connect *pig-host*
					   *pig-port*
					   :protocol :datagram
					   :element-type '(unsigned-byte 8)))
	   (message (apply #'osc:encode-message (cons address (list data)))))
      (unwind-protect
	  (progn
	    (usocket:socket-send socket message (length message))
	    (format t "PIG: ~A  data ~A~%" address data)
	    (when socket (usocket:socket-close socket))))))

  (defun pig-ping ()
    (pig-send "ping"))


  (defun pig-load-smf (name)
    (let ((filename (resolve-midi-filename name)))
      (if filename
	  (progn
	    (setf *pig-midi-filename* filename)
	    (pig-send "exec" (sformat "op ~A, load, ~A" *pig-player-op* filename))
	    (format t "Pigiron MIDI filename is: ~A~%" filename)
	    (sleep 0.1)
	    t)
	(progn
	  (cyco-error (sformat "Pigiron can not load MIDI file: ~A~%" name))
	  nil))))
  

  (defun stop ()
    (pig-send "exec" (sformat "op ~A, stop" *pig-player-op*))
    (sleep *pig-sleep-time*))


  (defun play (&optional name)
    (stop)
    (if name
	(pig-load-smf name))
    (pig-send "exec" (sformat "op ~A, play" *pig-player-op*)))
      

  (defun output-channel (inst)
    (setf *pig-output-channels* '())
    (dolist (q (->list inst))
      (let ((c (channel q :resolve)))
	(push c *pig-output-channels*)))
    (pig-send "exec" (sformat "deselect-all-channels ~A" *pig-distributor-op*))
    (dolist (c *pig-output-channels*)
      (sleep 0.1)
      (pig-send "exec" (sformat "select-channel, ~A, ~A" *pig-distributor-op* c)))
    *pig-output-channels*)


  (defun pigout (&rest inst)
    "Alias for pig:output-channel"
    (funcall #'output-channel inst))

  (defun pigoff ()
    "Alias for pig:output-channel nil" 
    (output-channel nil))
  
  
  (defun pig-notes (keylist &key (vel 64)(dur 1.0))
    (let ((keys (keynumber (->list keylist)))
	  (v (truncate (logand vel #x7F))))
      (dolist (k keys)
	(dolist (c (->list *pig-output-channels*))
	  (let ((status (->status c #x90)))
	    (midi->pig status k v))))
      (sleep dur)
      (dolist (k keys)
	(dolist (c (->list *pig-output-channels*))
	  (let ((status (->status c #x80)))
	    (midi->pig status k 0))))))
      

  (defun pig-cc (controller value)
    (let ((ctrl (get-controller-number controller))
	   (val (truncate (logand value #x7f))))
       (dolist (c (->list *pig-output-channels*))
	 (midi->pig (->status c #xB0) ctrl val))))
	  

  (defun pig-program (pnumber)
    (let ((n (truncate (logand pnumber #x7f))))
      (dolist (c (->list *pig-output-channels*))
	(midi->pig (->status c #xC0) n))))


  (defun pig-bend (normal-bend)
    (let* ((bytes (bend->midi-data normal-bend))
	   (msb (aref bytes 0))
	   (lsb (aref bytes 1)))
      (dolist (c (->list *pig-output-channels*))
	(midi->pig (->status c #xE0) msb lsb))))


  (defun pig-sysex (bytes &optional raw)
    (let ((lst (if raw
		   (->list bytes)
		 (append '(#xF0) (->list bytes) '(#xF7)))))
    (apply #'midi->pig lst))) )


;;; Conveniences functions
;;;

(defun s ()
  (stop))

(defun p (&optional name)
  (play name))

(defun play-main ()
  "Play project's main MIDI file."
  (if *project*
      (let ((fname (property *project* :midi-filename)))
	(if fname
	    (progn 
	      (play fname)
	      fname)
	  (cyco-warning "Project :MIDI-FILENAME property is NIL.")))
    (cyco-error "No current project. *PROJECT* is NIL.")))

(defun play-section (&optional section)
  "Play section's most recent MIDI file.
Section defaults to the project's current-section."

  (let* ((s (or section (and *project* (property *project* :current-section))))
	 (fname (and s (property s :midi-filename))))
    (if fname
	(progn
	  (play fname)
	  fname)
      (cyco-warning "No current section defined."))))
    

(defun lpp (&optional name)
  (lpf)
  (play name))


(load-plugin-file "docs")

