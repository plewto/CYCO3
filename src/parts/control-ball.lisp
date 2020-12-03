;;;; CYCO control-ball.lisp

(in-package :cyco-part)

(constant +control-ball-properties+
	  (append +part-properties+
		  '(:controller
		    :shift
		    :render-once
		    :cue-function
		    :start-cue
		    :end-cue
		    :time-interval
		    :value-pattern
		    :reset-on-repeat
		    :trim
		    :initial-value
		    :initial-value-time-shift
		    :final-value
		    :final-value-time-shift
		    
		    )))
		    

(defclass control-ball (part) nil)

(defgeneric control-ball-p (object))
(defmethod control-ball-p ((object t)) nil)
(defmethod control-ball-p ((object control-ball)) t)
(defgeneric control-ball-steps (object))

(labels ((bad-section-error (section part-name)
	    (cyco-type-error 'make-control-ball "Section or nil" section
			     (sformat "CONTROL-BALL ~A" part-name)))
	 
	 (no-project-error (part-name)
	    (cyco-composition-error 'make-control-ball
				    (sformat "No default project while creating CONTROL-BALL ~A"
					     part-name)))


	 (invalid-instrument-error (part-name)
	    (cyco-type-error 'make-control-ball "Instrument or list of instruments"
			     (sformat "Invalid instrument CONTROL-BALL ~A" part-name)))


	 (invalid-controller-error (controller part-name)
	    (cyco-type-error 'make-control-ball
			     "MIDI controller number :PRESSURE or :BEND"
			     controller
			     (sformat "CONTROL-BALL ~A" part-name)))
	 
	 
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

	 ;; nil
	 ;; pattern object
	 ;; (:RAMP amp1 amp2 )
	 ;; (:sawtooth amp1 amp2 :phase)
	 ;; (:triangle amp1 amp2 :phase)
	 ;; (:pulse amp1 amp2 :phase :width)
	 (create-pattern (control-ball pattern)
			 (if (or (null pattern)(pattern-p pattern))
			     (return-from create-pattern pattern))
			 (let* ((steps (control-ball-steps control-ball))
				(curve-type (car pattern))
				(args (append (cdr pattern)(list :steps steps))))
			   (cond ((eq curve-type :ramp)
				  (apply #'ramp args))
				 ((eq curve-type :sawtooth)
				  (apply #'sawtooth args))
				 ((eq curve-type :triangle)
				  (apply #'triangle args))
				 ((eq curve-type :pulse)
				  (apply #'pulse args))
				 (t (cyco-type-error 'make-control-ball
						     "curve type, one of :RAMP :SAWTOOTH :TRIANGLE or :PULSE"
						     curve-type
						     (sformat "CONTROL-BALL ~A" (name control-ball))))))) )

  (defun make-control-ball (name controller instruments start end &key
			  section
			  cuefn
			  shift
			  tempo unit bars beats subbeats
			  render-once
			  interval
			  pattern
			  trim
			  reset-on-repeat
			  remarks
			  initial
			  final)
    (let ((parent (validate-section name section))
	  (instrument-list (validate-instruments name instruments))
	  (controller-type (validate-controller name controller)))
      (if (not (and parent instrument-list controller-type))
	  (return-from make-control-ball nil))
      (let ((control-ball (make-instance 'control-ball
				  :properties +control-ball-properties+
				  :name name
				  :remarks (->string (or remarks ""))
				  :transient t)))
	(connect parent control-ball)
	(put control-ball :controller controller)
	(put control-ball :tempo tempo)
	(put control-ball :unit unit)
	(put control-ball :bars bars)
	(put control-ball :beats beats)
	(put control-ball :subbeats subbeats)
	(put control-ball :cue-function cuefn)
	(put control-ball :render-once render-once)
	(put control-ball :transposable nil)
	(put control-ball :reversible nil)
	(put control-ball :trim trim)
	(put control-ball :muted nil)
	(put control-ball :instruments instrument-list)
	(put control-ball :start-cue start)
	(put control-ball :end-cue end)
	(put control-ball :time-interval (or interval 's))
	(put control-ball :shift (metric-expression (or shift 0.0)))
	(put control-ball :reset-on-repeat reset-on-repeat)
	(let ((v (car initial))
	      (time-shift (or (cdr initial) 0)))
	  (put control-ball :initial-value v)
	  (put control-ball :initial-value-time-shift (and v time-shift)))
	(let ((v (car final))
	      (time-shift (or (cdr final) 0)))
	  (put control-ball :final-value v)
	  (put control-ball :final-value-time-shift (and v time-shift)))
	(put control-ball :value-pattern (create-pattern control-ball pattern))
	(reset control-ball)
	control-ball))) )

(setf (documentation 'make-control-ball 'function) +control-ball-docstring+)


(defmacro control-ball (name controller instruments start end &key
		      section
		      cuefn
		      shift
		      tempo unit bars beats subbeats
		      render-once
		      interval
		      pattern
		      trim
		      reset-on-repeat
		      remarks
		      initial
		      final)
  `(progn
     (part-banner (name ,section) ',name)
     (let ((control-ball (make-control-ball ',name ,controller ,instruments ,start ,end
			      :section ,section
			      :cuefn ,cuefn
			      :shift ,shift
			      :tempo  ,tempo 
			      :unit  ,unit 
			      :bars  ,bars 
			      :beats  ,beats 
			      :subbeats ,subbeats
			      :render-once ,render-once
			      :interval ,interval
			      :pattern ,pattern
			      :trim ,trim
			      :reset-on-repeat ,reset-on-repeat
			      :initial ,initial
			      :final ,final
			      :remarks ,remarks)))
       (defparameter ,name control-ball)
       control-ball)))
		  

(defmethod control-ball-steps ((control-ball control-ball))
  (let* ((cuefn (property control-ball :cue-function))
	 (start-time (funcall cuefn control-ball (property control-ball :start-cue)))
	 (end-time (funcall cuefn control-ball (property control-ball :end-cue)))
	 (time-delta (float (- end-time start-time)))
	 (interval (let* ((n (property control-ball :time-interval))
			  (scale (if (numberp n)
				     1.0
				   (beat-duration control-ball))))
		     (* scale (metric-expression n)))))
	(if (minusp time-delta)
	    0
	  (truncate (/ time-delta interval)))))

(defmethod transpose ((control-ball control-ball)(n t))
  control-ball)

(defmethod invert ((control-ball control-ball)(pivot t))
  control-ball)

(defmethod retrograde ((control-ball control-ball))
  control-ball)

(defmethod reset ((control-ball control-ball))
  (reset (property control-ball :value-pattern))
  control-ball)

(defmethod clone ((mother control-ball) &key new-name new-parent)
  (let* ((frmt (or new-name "~A"))
	 (name (->symbol (sformat frmt (name mother))))
	 (parent (or new-parent (parent mother)))
	 (initial-value (property mother :initial-value))
	 (initial-time-shift (property mother :initial-value-time-shift))
	 (initial (if initial-value (cons initial-value initial-time-shift)))
	 (final-value (property mother :final-value))
	 (final-time-shift (property mother :final-value-time-shift))
	 (final (if final-value (cons final-value final-time-shift)))
	 (daughter (make-control-ball name
			    (property mother :controller)
			    (property mother :instruments)
			    (property mother :start-cue)
			    (property mother :end-cue)
			    :section parent
			    :cuefn (property mother :cue-function)
			    :shift (property mother :shift)
			    :render-once (property mother :render-once)
			    :interval (property mother :time-interval)
			    :pattern (property mother :value-pattern)
			    :reset-on-repeat (property mother :reset-on-repeat)
			    :trim (property mother :trim)
			    :initial initial
			    :final final
			    :remarks (property mother :remarks))))
    (copy-time-signature mother daughter)
    daughter)) 

(setf (documentation 'control-ball 'function)
      (sformat "The CONTROL-BALL macro is a thin wrapper of the MAKE-CONTROL-BALL
      function.  The primary difference is the CONTROL-BALL binds the new object
      to the named name, while MAKE-CONTROL-BALL does not.   The name argument
      should be quoted for MAKE-CONTROL-BALL and unquoted for CONTROL-BALL. ~%~%~A"
	       +control-ball-docstring+))
