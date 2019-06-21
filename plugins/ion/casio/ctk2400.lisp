;;;; CYCO plugin ion casio ctk2400
;;;;
;;;; The CTK2400 is a consumer grade general MIDI instrument.  This one
;;;; was forsaken in near mint condition next to a dumpster by a departing
;;;; Washington University student.  It comes with a crappy stand and a
;;;; live-action LCD image of hands showing you which keys you are
;;;; pressing (I just look at my hands). There is some kind of sampler
;;;; functionality I haven't had time to investigate.  On the plus side
;;;; the vibes and organ sounds are not half bad and there are  over 400
;;;; ROM presets.
;;;;
;;;;    |
;;;;    +-- casio
;;;;    |    |
;;;;        


(instrument casio
	    :parent *root-instrument*
	    :channel (meta-channel :casio)
	    :transient nil)

(defmacro casio (name &key (bank nil)(program nil)(parent casio)(channel nil)
		       keynumber-map dynamic-map articulation-map remarks)
  `(let* ((prg (general-midi-program (or ,program ',name)))
	  (rem (or ,remarks (sformat "CASIO bank: ~A  program: ~A" ,bank prg)))
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
				  (format t "Use (?GENERAL-MIDI-PROGRAMS) for list of CASIO programs.~%")
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
