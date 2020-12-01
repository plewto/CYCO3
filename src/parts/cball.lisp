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
		    :reset-on-repeat
		    :initial-value
		    :initial-value-time-shift
		    :final-value
		    :final-value-time-shift)))
		    

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
		 (invalid-controller-error controller part-name))) )

  (defun make-cball (name controller instruments start end &key
			  section
			  cuefn
			  shift
			  tempo unit bars beats subbeats
			  render-once
			  increment
			  pattern
			  reset-on-repeat
			  remarks
			  initial
			  final)
    (let ((parent (validate-section name section))
	  (instrument-list (validate-instruments name instruments))
	  (controller-type (validate-controller name controller)))
      (if (not (and parent instrument-list controller-type))
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
	(put cball :time-increment (or increment 's))
	(put cball :shift (metric-expression (or shift 0.0)))
	(put cball :value-pattern pattern)
	(put cball :reset-on-repeat reset-on-repeat)
	(let ((v (car initial))
	      (time-shift (or (cdr initial) 0)))
	  (put cball :initial-value v)
	  (put cball :initial-value-time-shift (and v time-shift)))
	(let ((v (car final))
	      (time-shift (or (cdr final) 0)))
	  (put cball :final-value v)
	  (put cball :final-value-time-shift (and v time-shift)))
	(reset cball)
	cball))) )

(setf (documentation 'make-cball 'function) +cball-docstring+)

	
(defmacro cball (name controller instruments start end &key
		      section
		      cuefn
		      shift
		      tempo unit bars beats subbeats
		      render-once
		      increment
		      pattern
		      reset-on-repeat
		      remarks
		      initial
		      final)
  `(progn
     (part-banner (name ,section) ',name)
     (let ((cball (make-cball ',name ,controller ,instruments ,start ,end
			      :section ,section
			      :cuefn ,cuefn
			      :shift ,shift
			      :tempo  ,tempo 
			      :unit  ,unit 
			      :bars  ,bars 
			      :beats  ,beats 
			      :subbeats ,subbeats
			      :render-once ,render-once
			      :increment ,increment
			      :pattern ,pattern
			      :reset-on-repeat ,reset-on-repeat
			      :initial ,initial
			      :final ,final
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
	 (initial-value (property source :initial-value))
	 (initial-time-shift (property source :initial-value-time-shift))
	 (initial (if initial-value (cons initial-value initial-time-shift)))
	 (final-value (property source :final-value))
	 (final-time-shift (property source :final-value-time-shift))
	 (final (if final-value (cons final-value final-time-shift)))
	 (other (make-cball (->symbol frmt (name source))
			    (property source :controller)
			    (property source :instruments)
			    (property source :start-cue)
			    (property source :end-cue)
			    :section parent
			    :cuefn (property source :cue-function)
			    :shift (property source :shift)
			    :render-once (property source :render-once)
			    :increment (property source :time-increment)
			    :pattern (property source :pattern)
			    :reset-on-repeat (property source :reset-on-repeat)
			    :initial initial
			    :final final
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
