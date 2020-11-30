;;;; CYCO cball.lisp

(in-package :cyco-part)

(constant +cball-properties+
	  (append +part-properties+
		  '(:controller
		    :shift
		    :render-once
		    :cue-function
		    :start-cue
		    :end-cue
		    :time-increment
		    :value-pattern
		    :reset-on-repeat)))

(defclass cball (part) nil)


(defgeneric cball-p (object))
(defmethod cball-p ((object t)) nil)
(defmethod cball-p ((object cball)) t)


(labels ((bad-section-error (section part-name)
	    (cyco-type-error 'make-cball "Section or nil" section
			     (sformat "CBALL ~A" part-name)))
	 
	 (no-project-error (part-name)
	    (cyco-composition-error 'make-cball
				    (sformat "No default project while creating CBALL ~A"
					     part-name)))


	 (invalid-instrument-error (part-name)
	    (cyco-type-error 'make-cball "Instrument or list of instruments"
			     (sformat "Invalid instrument CBALL ~A" part-name)))


	 (invalid-controller-error (controller part-name)
	    (cyco-type-error 'make-cball
			     "MIDI controller number :PRESSURE or :BEND"
			     controller
			     (sformat "CBALL ~A" part-name)))
	 
	 
	 (validate-section (part-name section)
	     (cond ((section-p section) section)
		   ((and section (not (section-p section)))
		    (bad-section-error section part-name))
		   ((not (project-p *project*))
		    (no-project-error part-name))
		   (t (property *project* :current-section))))

	 
	 ;; Checks for instrument or list of instruments.
	 ;; Returns list of instruments
	 ;;
	 (validate-instruments (part-name instruments)
	     (let ((instrument-list (->list instruments)))
	       (if (every #'instrument-p instrument-list)
		   instrument-list
		 (invalid-instrument-error part-name))))

	 (validate-controller (part-name controller)
	     (or (and (numberp controller)(<= 0 controller)(< controller 128))
		 (eq controller :pressure)
		 (eq controller :bend)
		 (invalid-controller-error controller part-name)))

	 (validate-cue-points (part-name start end increment)
	     (or (and start end increment)
		 (cyco-type-error
		  'make-cball
		  "Values must be supplied for :START, :END and :INCREMENT"
		  (sformat ":START = ~A  :END = ~A   :INCREMENT = ~A" start end increment)
		  (sformat "CBALL ~A" part-name)))) )

  (defun make-cball (name controller instruments &key
			  section
			  cuefn
			  shift
			  tempo unit bars beats subbeats
			  render-once
			  start
			  end
			  increment
			  pattern
			  reset-on-repeat
			  remarks)
    (let ((parent (validate-section name section))
	  (instrument-list (validate-instruments name instruments))
	  (controller-type (validate-controller name controller)))
      (if (not (and parent instrument-list controller-type))
	  (return-from make-cball nil))
      (if (not (validate-cue-points name start end increment))
	  (return-from make-cball nil))
      (let ((cball (make-instance 'cball
				  :properties +cball-properties+
				  :name name
				  :remarks (->string (or remarks ""))
				  :transient t)))
	(connect parent cball)
	(put cball :controller controller)
	(put cball :tempo tempo)
	(put cball :unit unit)
	(put cball :bars bars)
	(put cball :beats beats)
	(put cball :subbeats subbeats)
	(put cball :cue-function cuefn)
	(put cball :render-once render-once)
	(put cball :transposable nil)
	(put cball :reversible nil)
	(put cball :muted nil)
	(put cball :instruments instrument-list)
	(put cball :start-cue start)
	(put cball :end-cue end)
	(put cball :time-increment increment)
	(put cball :shift (metric-expression (or shift 0.0)))
	(put cball :value-pattern (or pattern (line :of 0.0)))
	(put cball :reset-on-repeat reset-on-repeat)
	(reset cball)
	cball))) )

(setf (documentation 'make-cball 'function) +cball-docstring+)

	
(defmacro cball (name controller instruments  &key
		      section
		      cuefn
		      shift
		      tempo unit bars beats subbeats
		      render-once
		      start
		      end
		      increment
		      pattern
		      reset-on-repeat
		      remarks)
  `(progn
     (part-banner (name ,section) ',name)
     (let ((cball (make-cball ',name ,controller ,instruments
			      :section ,section
			      :cuefn ,cuefn
			      :shift ,shift
			      :tempo  ,tempo 
			      :unit  ,unit 
			      :bars  ,bars 
			      :beats  ,beats 
			      :subbeats ,subbeats
			      :render-once ,render-once
			      :start ,start
			      :end ,end
			      :increment ,increment
			      :pattern ,pattern
			      :reset-on-repeat ,reset-on-repeat
			      :remarks ,remarks)))
       (defparameter ,name cball)
       cball)))
		  
  
(defmethod transpose ((cball cball)(n t))
  cball)

(defmethod invert ((cball cball)(pivot t))
  cball)

(defmethod retrograde ((cball cball))
  cball)

(defmethod reset ((cball cball))
  (reset (property cball :value-pattern))
  cball)

(defmethod reset ((cball cball))
  cball)

(defmethod clone ((source cball) &key new-name new-parent)
  (let* ((frmt (or new-name "~A"))
	 (parent (or new-parent (parent source)))
	 (other (make-cball (->symbol frmt (name source))
			    (property source :controller)
			    (property source :instruments)
			    :section parent
			    :cuefn (property source :cue-function)
			    :shift (property source :shift)
			    :render-once (property source :render-once)
			    :start (property source :start-cue)
			    :end (property source :end-cue)
			    :increment (property source :time-increment)
			    :pattern (property source :pattern)
			    :reset-on-repeat (property source :reset-on-repeat)
			    :remarks (property source :remarks))))
    (copy-time-signature source other)
    (dolist (sub-part (children source))
      (clone sub-part :new-name frmt :new-parent other))
    other)) 
    

(setf (documentation 'cball 'function)
      (sformat "The CBALL macro is a thin wrapper of the MAKE-CBALL
      function.  The primary difference is the CBALL binds the new object
      to the named name, while MAKE-CBALL does not.   The name argument
      should be quoted for MAKE-CBALL and unquoted for CBALL. ~%~%~A"
	       +cball-docstring+))

    
			   
