;;;; CYCO plugins ion yamaha mu100r
;;;;
;;;; The Yamaha MU100R is a general MIDI "rompler".  Unfortunately the unit
;;;; I have is missing the "VL" physical modeling expansion card which would
;;;; make it a hell of a lot more interesting.
;;;;
;;;; The module is 32-voice multi-timbrel with two separate MIDI inputs
;;;; for 32-channel reception.
;;;;
;;;; A master MU100R instrument is created and a macro defined for creating
;;;; general-midi instruments under it.  The macro's usage is the same as
;;;; with GENERAL-MIDI-INSTRUMENT.
;;;;

(instrument mu100r
	    :parent *root-instrument*
	    :channel (meta-channel :mu100r)
	    :transient nil)

(flet ((resolve-program-number (instrument-name program-name-or-number)
			       (cond ((and program-name-or-number (numberp program-name-or-number))
				      (1- program-name-or-number))
				     ((general-midi-program-p program-name-or-number)
				      (general-midi-program program-name-or-number))
				     ((general-midi-program-p instrument-name)
				      (general-midi-program instrument-name))
				     (t (cyco-warning "Invalid MU100r program"
						      (sformat "instrument-name ~A" instrument-name)
						      (sformat "program-name-or-number ~A" program-name-or-number)
						      (sformat "Using 0"))
					0))))
			 
  (defun make-mu100r-instrument (name &key (bank nil)(program nil)(parent mu100r)(channel nil)
  				      keynumber-map dynamic-map articulation-map remarks)   
    (let* ((rem (or remarks (sformat "MU100R bank: ~A  program: ~A" bank program)))
	   (new-instrument (make-instrument name
					    :parent parent
					    :transient t
					    :channel (and channel (meta-channel channel))
					    :program (resolve-program-number name program)
					    :bank bank
					    :keynumber-map keynumber-map
					    :articulation-map articulation-map
					    :dynamic-map dynamic-map
					    :remarks (->string rem))))
      (program-map! new-instrument #'(lambda (time &key bank program)
				       (if (eq program :doc)
					   (progn
					     (format t "Use (?GENERAL-MIDI-PROGRAMS) for list of MU100R programs.~%")
					     nil)
					 (let ((ci (channel-index new-instrument))
					       (bank (cond ((eq bank :default)
							    (property new-instrument :program-bank))
							   (bank bank)
							   (t nil)))
					       (pnum (cond ((eq program :default)
							    (property new-instrument :program-number))
							   (program program)
							   (t nil))))
					   (let ((event-list '()))
					     (if bank
						 (let ((msb (if (> bank #x7F) #x30 0))
						       (lsb (logand bank #x7f)))
						   (setf event-list (list (cons time (midi-control-change ci 0 msb))
									  (cons (+ time 0.00001) (midi-control-change ci 32 lsb))
									  (cons (+ time 0.00002) (midi-program-change ci (or pnum 0))))))
					       (setf event-list (list (cons time (midi-program-change ci (or pnum (program-number new-instrument)))))))
					     event-list)))))
      new-instrument)))


(defmacro mu100r (name &key (bank nil)(program nil)(parent mu100r)(channel nil)
		       keynumber-map dynamic-map articulation-map remarks)
  `(let ((new-instrument (make-mu100r-instrument ',name 
						 :bank ,bank 
						 :program ,program 
						 :parent ,parent 
						 :channel ,channel 
						 :remarks ,remarks
						 :keynumber-map ,keynumber-map
						 :dynamic-map ,dynamic-map
						 :articulation-map ,articulation-map)))
     (defparameter ,name new-instrument)
     new-instrument)) 

				     
				     
		
