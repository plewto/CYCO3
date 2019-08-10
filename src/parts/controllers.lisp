;;;; CYCO

(constant +controllers-documentation+
"Creates new CONTROLLERS instance.

 A Controllers generates MIDI bend, channel-pressure and controller events
either as single-point or multi-point curve.
 
Controllers do not directly have instruments or mute status, instead they 
 depend on their parent node to provide these parameters.  

 name         - Symbol
 parent       - Parent node, most likely another part.
 :render-once - flag
 :remarks     - String 
 :events      - Events are specified as a nested list of 'clauses'

event clauses:

 :time t1 t2
    Sets time range, arguments t1 and t2 must be in format required by
    the part's cue-function.
    The special value of '= for either argument uses the current value.

 :to tnew
     Like :time but takes the current t2 value as the initial time
     :to is a shorthand for :time t2 tnew.

 :steps n
    Number of events generated between times 1 and 2, default 16
    The actual event count is steps times number of instruments.

 :value v1 v2
   Sets curve value range (time1, v1), (time2, v2)
   The special value of = for either argument uses the current value.
   Values are expected to be normalized for MIDI controller and pressure
   evens, and signed-normalized for bend events.

 :type t
    Sets event type
       integer --> MIDI controller event, 0 <= n < 128.
       'touch  --> MIDI channel-pressure
       'bend   --> MIDI pitch-bend

 :curve c [param]
   Selects the curve type, the meaning of the optional param argument
   is dependent on the curve type.
   'point   - Creates a single event at (time2, value1), param not used.
   'line    - Generates line between points (time1,value1) and
              (time2,value2).  Param sets exponential degree, 0 < param.
   'saw     - Generates Sawtooth waves between (time1,value1) and
              (time2,value2), slope is inverted for value1>value2.
              param argument sets number of sawtooth cycles.
   'tri     - Generates triangle waves, param -> cycle count.
   'sin     - Generates sine waves, param -> cycle count.
   'cos     - Identical to sin except phase-shifted 90 degrees.
   'sin2    - Generates sin(x)+(1/2)*sin(2x) wave, param -> cycle count.
   'cos2    - Identical to sin2 but phase shifted 90 degrees.
   'lin+cos - Generates curve that is the sum of a line and a cosine
              waves.  The cosine amplitude is fixed. param -> cycle count.
   'random  - Generates random values between times t1 and t2, with
              values between v1 and v2.  param is not used.

 :blur b
    Blurring ratio, 0 <= b <= 1, See approximate function.

 :TRACE off
 :TRACE events
 :TRACE states
 :TRACE all     
     Selects debug trace modes.")

(defstruct controllers-state
  (source "")
  (time1 nil)
  (time2 nil)
  (steps nil)
  (value1 nil)
  (value2 nil)
  (event-type nil)
  (curve-type nil)
  (cycles 1.0)	  ;; used as degree for line curve.
  (blur nil))
  
(defmethod soft-reset ((state controllers-state))
  (setf (controllers-state-curve-type state) nil)
  state)

(defmethod reset ((state controllers-state))
  (soft-reset state)
  (setf (controllers-state-source state) nil)
  (setf (controllers-state-time1 state) '(1 1 1))
  (setf (controllers-state-time2 state) '(2 1 1))
  (setf (controllers-state-steps state) 16)
  (setf (controllers-state-value1 state) 0.0)
  (setf (controllers-state-value2 state) 1.0)
  (setf (controllers-state-event-type state) nil)
  (setf (controllers-state-curve-type state) nil)
  (setf (controllers-state-cycles state) 1.0)
  state)

(defmethod clone ((state controllers-state) &key new-name new-parent)
  (dismiss new-name new-parent)
  (make-controllers-state
   :source (controllers-state-source state)
   :time1 (controllers-state-time1 state)
   :time2 (controllers-state-time2 state)
   :steps (controllers-state-steps state)
   :value1 (controllers-state-value1 state)
   :value2 (controllers-state-value2 state)
   :event-type (controllers-state-event-type state)
   :curve-type (controllers-state-curve-type state)
   :cycles (controllers-state-cycles state)
   :blur (controllers-state-blur state)))

(constant +controllers-properties+
	  (append +part-properties+
		  '(:shift
		    :render-once)))

(constant +controllers-curve-types+
	  '(point line saw tri sin sin2 cos cos2 lin+cos random))

(defclass controllers (part)
  ((state
    :type controllers-state
    :accessor controllers-state
    :initform (let ((s (make-instance 'controllers-state)))
		(reset s)
		s))
   (events
   :type list   ; of controllers-states
   :accessor controllers-events
   :initform '())))

(global *trace-controllers-events* nil)
(global *trace-controllers-states* nil)

(labels ((trace-event
	  (part event)
	  (if *trace-controllers-events*
	      (let ((frmt "TRACE: ~A.~A event: ~A~%"))
		(format t "~A~%" +banner-bar2+)
		(format t frmt (name (parent part))(name part) event))))

	 (trace-state
	  (state)
	  (if *trace-controllers-states*
	      (progn 
		(format t "~A~%" +banner-bar2+)
		(format t "~A~%" state))))

	 (switch-trace-mode
	  (clause)
	  (let ((mode (or (second clause) 'all)))
	    (setf *trace-controllers-events* (or (eq mode 'events)(eq mode 'all)))
	    (setf *trace-controllers-states* (or (eq mode 'state)(eq mode 'all)))))
	 
	 (id-part
	   (part)
	   (sformat "Part ~A.~A" (name (parent part))(name part)))
  
	 (expect-n-arguments
	  (part event clause min max)
	  (let ((count (1- (length clause))))
	    (if (and (<= min count)(<= count max))
		t
	      (progn
		(cyco-warning
		 (if (= min max)
		     (sformat "Expected ~A argument(s), Encountered ~A" min count)
		   (sformat "Expected between ~A and ~A arguments, Encountered ~A"
		 	    min max count))
		 (id-part part)
		 (sformat "Event  : ~A" event)
		 (sformat "Clause : ~A" clause)
		 nil)))))
	 
	 (process-time 			; :time ti t2
	  (part state event clause)
	  (if (expect-n-arguments part event clause 2 2)
	      (let* ((cuefn (property part :cue-function))
		     (arg1 (second clause))
		     (arg2 (third clause))
		     (t1 (if (eq arg1 '=)
			     (or (controllers-state-time1 state) 0.0)
			   (funcall cuefn part arg1)))
		     (t2 (if (eq arg2 '=)
			     (or (controllers-state-time2 state) 0.0)
			   (funcall cuefn part arg2))))
		(setf (controllers-state-time1 state) (min t1 t2))
		(setf (controllers-state-time2 state) (max t1 t2))
		t)
	    nil))

	 (process-time-to		; :to t2
	  (part state event clause)	; Shortcut for :time t2 q, where t2 is previous time2
	  (if (expect-n-arguments part event clause 1 1)
	      (let* ((cuefn (property part :cue-function))
		     (arg2 (second clause))
		     (t2 (funcall cuefn part arg2))
		     (t1 (or (controllers-state-time2 state) 0.0)))
		(setf (controllers-state-time1 state) (min t1 t2))
		(setf (controllers-state-time2 state) (max t1 t2))
		t)
	    nil))
	  
	 (process-step-count		; :steps n
	  (part state event clause)	;   n > 0
	  (if (expect-n-arguments part event clause 1 1)
	      (let* ((n (second clause)))
		(if (and (integerp n)(plusp n))
		    (setf (controllers-state-steps state) n)
		  (cyco-warning
		   (sformat "Expected positive integer, Encountered ~A" n)
		   (id-part part)
		   (sformat "Event  ~A" event)
		   (sformat "Clause ~A" clause))))))

	 (process-value 		; :value v1 v2
	  (part state event clause)	;    v1, v2 normalized float
	  (if (expect-n-arguments part event clause 2 2)
	      (let* ((arg1 (second clause))
		     (arg2 (third clause)))
		(if (every #'(lambda (q)
			       (or (eq q '=)
				   (and (numberp q)
					(<= -1 q)
					(<= q +1))))
			   (list arg1 arg2))
		    (progn 
		      (if (not (eq arg1 '=))
			  (setf (controllers-state-value1 state) arg1))
		      (if (not (eq arg2 '=))
			  (setf (controllers-state-value2 state) arg2)))
		  (cyco-warning "Expected signed-normalized value"
				(id-part part)
				(sformat "Event   ~A" event)
				(sformat "Clause  ~A" clause))))))

	 (process-event-type		; :type q
	  (part state event clause)
	  (if (expect-n-arguments part event clause 1 1)
	      (let* ((arg (second clause))
		     (value (cond ((and (integerp arg)
					(<= 0 arg)
					(<= arg 127)) arg)
				  ((eq arg 'touch) arg)
				  ((eq arg 'bend) arg)
				  (t (cyco-warning
				      "Expected MIDI controller number or symbols 'touch or 'bend"
				      (sformat "Encountered ~A" arg)
				      (id-part part)
				      (sformat "Event   ~A" event)
				      (sformat "Clause  ~A" clause))
				     nil))))
		(setf (controllers-state-event-type state) value))))


	 (process-event-curve		; :curve shape [param]
	  (part state event clause)
	  (if (expect-n-arguments part event clause 1 2)
	      (let* ((arg1 (second clause))
		     (shape (and (member arg1 +controllers-curve-types+) arg1)))
		(if shape
		    (setf (controllers-state-curve-type state) shape)
		  (cyco-warning
		   (sformat "Expected one of ~A,  encountered ~A" +controllers-curve-types+ arg1)
		   (id-part part)
		   (sformat "Event  ~A" event)
		   (sformat "Clause ~A" clause)))
		(let ((cycles (third clause)))
		  (cond ((eq cycles '=)
			 (setf (controllers-state-cycles state)
			       (or (controllers-state-cycles state) 1.0)))
			((and (numberp cycles)(plusp cycles))
			 (setf (controllers-state-cycles state) (float cycles)))
			(cycles (cyco-warning
				 (sformat "Expected positive cycle, encountered ~A" cycles)
				 (id-part part)
				 (sformat "Event  ~A" event)
				 (sformat "Clause ~A" clause))))))))
		       
	 (process-blur
	  (part state event clause)
	  (if (expect-n-arguments part event clause 1 1)
	      (let* ((arg (second clause)))
		(if (and (<= 0 arg)(<= arg 1))
		    (setf (controllers-state-blur state) arg)
		  (cyco-warning
		   (sformat "Expected float between 0.0 and 1.0, encountered ~A" arg)
		   (id-part part)
		   (sformat "Event   ~A" event)
		   (sformat "Clause  ~A" clause))))))
	 
	 (dispatch-command
	  (event clause part state)
	  (let ((command (car clause)))
	    (cond ((eq command :time)	; :time t1 t2
		   (process-time part state event clause))
		  ((eq command :to)	; :to t2
		   (process-time-to part state event clause))
		  ((eq command :steps)	; :steps n
		   (process-step-count part state event clause))
		  ((eq command :value)  ; :value v1 v2
		   (process-value part state event clause))
		  ((eq command :type)   ; :type q cycles
		   (process-event-type part state event clause))
		  ((eq command :curve)  ; :curve shape [param]
		   (process-event-curve part state event clause))
		  ((eq command :blur)    ;:blur n
		   (process-blur part state event clause))
		  (t (cyco-warning
		      (sformat "Undefined controllers event command: ~A" command)
		      (id-part part)
		      (sformat "Event   ~A" event)
		      (sformat "Clause  ~A" clause))) )))

	 (real-event-p
	  (state)
	  (and (controllers-state-time1 state)
	       (controllers-state-value1 state)
	       (controllers-state-event-type state)
	       (controllers-state-curve-type state)))
	 
	 (process-events
	  (part events)
	  (let ((acc '())
	 	(state (controllers-state part)))
	    (reset state)
	    (block outerblock
	      (dolist (event events)
		(soft-reset state)
		(trace-event part event)
		(setf (controllers-state-source state) (->string event))
		(block innerblock
		  (dolist (clause (partition-list event))
		    (let ((command (car clause)))
		      (cond ((eq command :trace)
			     (switch-trace-mode clause))
			    ((eq command :break)
			     (return-from outerblock nil))
			    ((eq command :stop)
			     (return-from innerblock nil))
			    ((eq command :reset)
			     (reset state)
			     (return-from innerblock nil))
			    (t (dispatch-command event clause part state))))))
		(if (real-event-p state)
		    (progn 
		      (push (clone state) acc)
		      (trace-state state)))))
	    acc)))

  (defun make-controllers (name parent-part &key remarks events)
    (let* ((controllers (make-instance 'controllers
				 :name name
				 :remarks (->string (or remarks ""))
				 :properties +controllers-properties+)))
      (put controllers :transposable nil)
      (put controllers :reversible nil)
      (put controllers :muted nil)
      (connect parent-part controllers)
      (init-time-signature controllers)
      (setf (controllers-events controllers)
      	    (reverse (process-events controllers (->list events))))
      controllers)) )

(setf (documentation 'make-controllers 'function) +controllers-documentation+)

(defmacro controllers (name parent-part &key remarks events)
  "Identical to make-controllers but binds part object to symbol name."
  `(progn
     (part-banner (name ,parent-part) ',name)
     (let ((new-controllers-part (make-controllers ',name ,parent-part :remarks ,remarks :events ,events)))
       (defparameter ,name new-controllers-part)
       new-controllers-part)))

(defmethod clone ((source controllers) &key new-name new-parent)
  (let* ((name (->symbol (sformat (or new-name "~A") (name source))))
	 (parent-part (or new-parent (parent source)))
	 (new-controllers-part (make-controllers name parent-part :remarks (remarks source)))
	 (events '()))
    (setf (controllers-state new-controllers-part)(clone (controllers-state source)))
    (dolist (event (controllers-events source))
      (push (clone event) events))
    (setf (controllers-events new-controllers-part)(reverse events))
    new-controllers-part))


