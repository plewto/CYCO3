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
	    :parent +root-instrument+
	    :channel (meta-channel :mu100r)
	    :transient nil)

(defmacro mu100r (name &key (bank nil)(program nil)(parent mu100r)(channel nil)
		       keynumber-map dynamic-map articulation-map remarks)
  
  `(let* ((prg (general-midi-program (or ,program ',name)))
	  (rem (or ,remarks (sformat "MU100R bank: ~A  program: ~A" ,bank prg)))
	  (inst (make-instrument ',name
				 :parent ,parent
				 :transient t
				 :channel (and ,channel (meta-channel ,channel))
				 :program prg
				 :bank ,bank
				 :keynumber-map ,keynumber-map
				 :articulation-map ,articulation-map
				 :dynamic-map ,dynamic-map
				 :remarks (->string rem))))
     (program-map! inst #'(lambda (time &key bank program)
			    (if (eq program :doc)
				(progn
				  (format t "Use (?GENERAL-MIDI-PROGRAMS) for list of MU100R programs.~%")
				  nil)
			      (let ((ci (channel-index inst))
				    (bank (cond ((eq bank :default)
						 (property inst :program-bank))
						(bank bank)
						(t nil)))
				    (pnum (cond ((eq program :default)
						 (property inst :program-number))
						(program program)
						(t nil))))
				(if bank
				    (let ((msb (if (> bank #x7F) #x30 0))
					  (lsb (logand bank #x7f)))
				      (list (cons time (midi-control-change ci 0 msb))
					    (cons (+ time 0.001) (midi-control-change ci 32 lsb))
					    (cons (+ time 0.002) (midi-program-change ci (or pnum 0)))))
				  (list (cons time (midi-program-change ci (or pnum 0)))))))))
       (defparameter ,name inst)
       inst))
			 
		  
			 
	  
		
