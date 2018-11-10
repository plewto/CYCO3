;;;; CYCO
;;;;
;;;; EPART is a Part with explicit event specifications.
;;;; 
;;;; Any number of instruments may be used with an Epart.  By default multiple
;;;; instruments are converted to an INSTRUMENT-LAYER pattern and are used in
;;;; parallel, however any Pattern of instruments may be used.
;;;; 
;;;; An Epart may generate note events and has a rich set of options for
;;;; strumming chords.   They may also generate simple touch, MIDI controller,
;;;; bend and program events.
;;;; 
;;;; Events are specified as a nested list of event commands.
;;;; 
;;;;     ((:command n :command n ...)
;;;;      (:command n :command n ...)
;;;;      ...........................)
;;;; 
;;;; Where each clause within a single event sub-list has the form
;;;; 
;;;;      :command [arguments..]
;;;; 
;;;; and :command is one of the keywords below. Argument counts are between 
;;;; 0 and 3, depending on the specific command.
;;;; 
;;;; :TRACE off
;;;; :TRACE events
;;;; :TRACE states
;;;; :TRACE all     
;;;;     Turns debug trace mode on/off 
;;;; 
;;;; :BREAK
;;;;     Skips evaluation of remaining events.
;;;; 
;;;; :STOP
;;;;     Skips remainder of current event.
;;;; 
;;;; :RESET
;;;;     Resets all parameters to initial condition.
;;;;
;;;; :TIME argument
;;;; :TIME argument-list
;;;;     Specifies event time.  The accepted argument format is dependent on
;;;;     the part's cuing-function.  Time remains in effect for all future
;;;;     events until either:
;;;;     1) It is explicitly changed.
;;;;     3) On reset.
;;;; 
;;;; :KEY k
;;;;     Creates a note event.
;;;;     The k argument is typically a keynumber, but it need not be so long as
;;;;     each instrument's keynumber-map is able to convert it to a keynumber.
;;;; 
;;;;     Keynumbers are not saved between events.
;;;; 
;;;; :CHORD name
;;;; :CHORD template
;;;;     Selects chord type, either by name or as a list of key offsets.
;;;; 
;;;;     :chord [maj]   
;;;;     :chord (0 4 7)
;;;; 
;;;;     Both select a simple major triad (assuming the part's chord-model
;;;;     defines [maj]).  The chord remains in effect until explicitly
;;;;     changed.
;;;;     
;;;; :INVERSION n
;;;;     Sets chord inversion, n should be a small signed integer.
;;;;     Positive values rotate the chord template right.
;;;; 
;;;;     (0 3 7)  inversion 0  --> (0 3 7)
;;;;     (0 3 7)  inversion 1  --> (3 7 12)
;;;;     (0 3 7)  inversion 2  --> (7 12 15)
;;;;     (0 3 7)  inversion -1 --> (-5 0 3)
;;;; 
;;;;     Inversion remains in effect until explicitly changed.
;;;; 
;;;; :OCTAVE n
;;;;     Inserts new note into chord template n-octaves above the initial
;;;;     note. 
;;;; 
;;;;     (0 3 7)  octave 0  --> (0 3 7)
;;;;     (0 3 7)  octave 1  --> (0 3 7 12)
;;;;     (0 3 7)  octave -1 --> (0 3 7 -12)
;;;; 
;;;;     Octave is applied after inversion and remains in effect until
;;;;     explicitly changed. 
;;;; 
;;;; :STRUM metric-expression
;;;;     Sets strum delay time between each note of a chord, I.E. strums the chord.
;;;; 
;;;;     For delay d and time t, chord notes are produced at times
;;;;     t, t+d, t+2d, t+3d, ...
;;;; 
;;;;     strum delay remains in effect until explicitly changed.
;;;; 
;;;; :STRUM* scale
;;;;     Adds strum delay acceleration.
;;;;
;;;;     For scale s, delay d, and time t, chord notes are produced at times:
;;;;     t, t+d, t+d*s, t+d*s^2, t+d*s^3, ...
;;;; 
;;;;     Acceleration remains in effect until explicitly changed. 
;;;;    
;;;; :DIRECTION d
;;;; :DIRECTION list 
;;;;     Sets strum direction.
;;;; 
;;;;     Valid directions are
;;;;         :down - play chord in note order
;;;;         :up   - reverse note order
;;;;         :dice - randomly select :up or :down
;;;;         :random - select notes randomly.
;;;; 
;;;;     The strum direction is a Cycle pattern
;;;; 
;;;;     :DIRECTION :down
;;;;     Play all chords in note order.
;;;; 
;;;;     :DIRECTION (:down :up :dice)
;;;;     Play first chord in note order.
;;;;     Play every second chord in reverse order.
;;;;     Randomly select :up or :down for every third chord.
;;;; 
;;;;     Direction remains in effect until explicitly changed.
;;;;
;;;; :GRACE key delay 
;;;;    Play grace note
;;;;    key    - key number
;;;;    delay  - metric expression
;;;;    Grave notes ignore chord-template
;;;;
;;;; :GRACE-AMP-SCALE n
;;;; :GRACE-ARTICULATION ex
;;;;
;;;; :AMP* s
;;;;     Scale dynamic of each chord note by s.
;;;;     For nominal dynamic a and scale s, note amplitudes are:
;;;; 
;;;;     a, a*s, a*s^2, a*s^3, ...
;;;; 
;;;;     See also :AMP-RANGE and :AMP-BLUR.
;;;;     AMP* remains in effect until explicitly changed.
;;;; 
;;;; :END-TOGETHER no
;;;; :END-TOGETHER yes
;;;;     Selects if strummed chord notes are to end at the same time or have
;;;;     staggered endings.
;;;; 
;;;;     end-together remains in effect until explicitly changed.
;;;; 
;;;; :DUR metric-expression|number
;;;;     Sets note duration.
;;;;     Duration remains in effect until explicitly changed.
;;;;     If duration is a non-negative number it is used as the absolute
;;;;     note duration, unless the instruments articulation-map changes it.
;;;;     If the duration is a metric-expression, it's value is scaled by 
;;;;     the current tempo.
;;;;
;;;; :AMP dynamic
;;;; :AMP list  (converted to cycle)
;;;;    Sets nominal note dynamic level pattern
;;;;    Dynamic level remains in effect until explicitly changed.
;;;;
;;;; :CRES start end count
;;;;    Creates dynamic change from start to end over the next
;;;;    count events.  start and end are dynamic values, count is
;;;;    an integer.  The special value of = for start indicates
;;;;    the current amplitude.
;;;;
;;;; :AMP-BLUR s
;;;;    Randomizes dynamic values for each new key-event
;;;;    A value of s=0.1 blurs dynamic by up to 10 percent.
;;;; 
;;;;    Blur remains in effect until explicitly changed.
;;;; 
;;;; :AMP-RANGE min max
;;;;    Sets minimum and maximum dynamic values. Limiting is applied after amp+,
;;;;    amp*, amp-blur and any strum scaling.  Both min and max are dynamic
;;;;    values and remain in effect until explicitly changed.
;;;; 
;;;; :TOUCH n
;;;;    Creates a single MIDI after-touch event for each instrument.
;;;;    0.0 <= n <= 1.0
;;;; 
;;;; :BEND b
;;;;    Creates a single MIDI bend event for each instrument.
;;;;    -1.0 <= b <= +1.0
;;;; 
;;;; :CTRL c
;;;;    Sets MIDI controller number.
;;;;    Where c is an integer between 0 and 127 inclusive.
;;;;    The controller number remains in effect until explicitly changed.
;;;; 
;;;; :CC n
;;;;   Creates single MIDI controller event for each instrument, using
;;;;   controller number established by CTRL.
;;;; 
;;;;   0.0 <= n <= 1.0
;;;; 
;;;; :PROGRAM p
;;;;    Creates simple program-change events for each instrument.
;;;;    The expected format for p is dependent on the instruments
;;;;    program-map functions.
;;;; 
;;;;    A value of default should cause the instruments to generate their
;;;;    default programs.   Numeric p values are typically interpreted as
;;;;    explicit MIDI program numbers."	  
;;;;
;;;; :BANK b p
;;;;    Creates bank and program-change events for each instrument.
;;;;    The expected format for bank b and program p is dependent
;;;;    on the instruments program-map functions.
;;;; 
;;;;    A value of default should cause the instruments to generate their
;;;;    default bank/programs.   Numeric p values are typically interpreted as
;;;;    explicit MIDI program numbers."	  
;;;; 

(constant +epart-properties+
	  (append +part-properties+
		  '(:shift		; shift intiial time relative to section
		    :render-once)))

(defclass epart (part)
  ((state
    :type epart-state
    :accessor epart-state
    :initform (make-epart-state))
   (events				
    :type list   ; list of states
    :accessor epart-events
    :initform '())))

;; Trace flag, if true print epart events as they are evaluated.
(global *trace-epart-events* nil) 

;; Trace flag, if true display epart-state after evaluating each event-list.
(global *trace-epart-states* nil)


;; Creates new epart and interprets the events list.
;; The primary task is to convert event list contents into a list of
;; epart-state objects.   Final conversion to MIDI events is taken care of
;; separately during rendering stage.
;;
(labels ((validate-section
	  (part-name section)
	  (let ((s (cond ((section-p section)
			  section)
			 ((and section (not (section-p section)))
			  (cyco-type-error 'make-epart '(section nil) section)
			  nil)
			 ((not (project-p *project*))
			  (cyco-composition-error
			   'make-epart
			   (sformat "No default project while creating epart ~A" part-name))
			  nil)
			 (t (property *project* :current-section)))))
	    s))

	 ;; Checks that instruments argument is valid.
	 ;;   1) A single instrument --> converted to list --> 2
	 ;;   2) A list of instruments --> converted to INSTRUMENT-LAYER pattern.
	 ;;   3) A Pattern of instruments.
	 ;; Returns Pattern.
	 (validate-instruments
	  (part-name instruments)
	  (cond
	   ((pattern-p instruments)
	    instruments) ;; does not check pattern elements
	   ((listp instruments)
	    (or (and (every #'instrument-p instruments)
	 	     (instrument-layer :of instruments))
	 	(cyco-type-error 'make-epart "List of instruments"
				 instruments
	 	       		 (sformat "part name is ~A" part-name))))
	   ((instrument-p instruments)
	    (instrument-layer :of (->list instruments)))
	   (t (cyco-type-error 'make-epart
	 		       "Instrument or list of instruments"
	 		       instruments
	 		       (sformat "part name is ~A" part-name)))))

	 ;; Debug, prints event-list prior to evaluating it.
	 ;;
	 (trace-event
	  (part event)
	  (if *trace-epart-events*
	      (let ((frmt "TRACE: ~A.~A event: ~A~%"))
		(format t "~A~%" +banner-bar2+)
		(format t frmt
			(name (parent part))
			(name part)
			event))))

	 ;; Debug, prints epart-state after each event-list is evaluated.
	 ;;
	 (trace-state
	  (state)
	  (if *trace-epart-states*
	      (progn
		(format t "~A~%" +banner-bar2+)
		(format t "~A~%" state))))

	 (part-id
	  (part)
	  (sformat "Section.Part -->  ~A.~A" (name (parent part))(name part)))
	 
	 ;; Assert event clause has n arguments.
	 ;; Returns nil on error
	 (expect-n-arguments
	  (part event clause n)
	  (let ((count (1- (length clause))))
	    (if (not (= count n))
		(progn
		  (cyco-warning
		   (sformat "Expected ~A arguments, Encountered ~A" n count)
		   (part-id part)
		   (sformat "Event  ~A" event)
		   (sformat "Clause ~A" clause))
		  nil)
	      t)))

	 ;; Test that nth clause argument passes test.
	 ;; For non-error return argument
	 ;; On error return default.
	 (expect-nth-argument
	  (part event clause pos label test default)
	  (let ((arg (nth pos clause)))
	    (if (not (funcall test arg))
		(progn
		  (cyco-warning
		   (sformat "Expected ~A,  Encountered ~A" label arg)
		   (part-id part)
		   (sformat "Event  ~A" event)
		   (sformat "Clause ~A" clause))
		  default)
	      arg)))

	 (expect-metric-expression
	  (part event clause pos default)
	  (expect-nth-argument
	   part event clause pos "metric-expression"
	   #'metric-expression-p default))

	 (expect-dynamic-value
	  (part event clause pos default)
	  (expect-nth-argument
	   part event clause pos "dynamic-value"
	   #'dynamic-p default))
	 
	 (expect-normalized-value
	  (part event clause pos &optional (default 0.0))
	  (expect-nth-argument
	   part event clause pos "normalized value"
	   #'(lambda (q)(and (numberp q)(<= 0 q)(<= q 1)))
	   default))

	 (expect-signed-normalized-value
	  (part event clause pos &optional (default 0.0))
	  (expect-nth-argument
	   part event clause pos "signed normalized value"
	   #'(lambda (q)(and (numberp q)(<= -1 q)(<= q +1)))
	   default))

	 (expect-integer
	  (part event clause pos &key (min -1e6)(max 1e6)(default 0))
	  (truncate (expect-nth-argument
	 	     part event clause pos
	 	     (sformat "integer in range (~A,~A)" min max)
	 	     #'(lambda (q)(and (integerp q)(<= min q)(<= q max)))
	 	     default)))

	 (expect-float
	  (part event clause pos &key (min -1e6)(max +1e6)(default 0.0))
	  (float (expect-nth-argument
	 	  part event clause pos
	 	  (sformat "float in range (~A,~A)" min max)
	 	  #'(lambda (q)(and (numberp q)(<= min q)(<= q max)))
	 	  default)))

	 (process-time                  ; :time args
	  (part event clause state)     ; arg format dependent on cue function
	  (if (expect-n-arguments part event clause 1)
	      (let* ((cuefn (property part :cue-function))
	 	     (args (second clause))
	 	     (time (funcall cuefn part args)))
	 	(setf (epart-state-time state) (float time))
	 	state)
	    nil))

	 (process-key 		        ; :key k
	  (part event clause state)
	  (if (expect-n-arguments part event clause 1)
	      (progn 
		(setf (epart-state-key state)(second clause))
		state)
	    nil))

	 (process-chord-template	; :chord name
	  (part event clause state)	; :chord template-list
	  (if (expect-n-arguments part event clause 1)
	      (let* ((id (second clause))
		     (model (property part :chord-model))
		     (template (cond ((and (listp id)(every #'integerp id))
				      id)
				     ((and (symbolp id)(defines-chord-p model id))
				      (chord-template model id))
				     (t (cyco-warning
					 (sformat "Invalid chord ~A" id)
					 (part-id part)
					 (sformat "Event ~A" event)
					 "Using default '(0)")
					'(0)))))
		(setf (epart-state-chord-template state) template)
		state)
	    nil))

	 (process-chord-inversion	; :inversion degree
	  (part event clause state)
	  (let ((degree (expect-integer part event clause 1 :min -16 :max 16)))
	    (setf (epart-state-chord-inversion state) degree)
	    state))

	 (process-chord-octave          ; :octave n
	  (part event clause state)
	  (let ((degree (expect-integer part event clause 1 :min -2 :max 4)))
	    (setf (epart-state-chord-octave state) degree)
	    state))

	 (process-strum-delay	        ; :strum metric-expression
	  (part event clause state)
	  (let ((mexp (expect-metric-expression part event clause 1 0.0)))
	    (setf (epart-state-strum-delay state) (metric-expression mexp))
	    state))
		       
	 (process-strum-acceleration 	; :strum* 0<float
	  (part event clause state)
	  (let ((acl (expect-float part event clause 1 :min 1/4 :max 4 :default 1.0)))
	    (setf (epart-state-strum-acceleration state) acl)
	    state))

	 (process-strum-direction 	; :direction d
	  (part event clause state)	; :direction list
	  (if (expect-n-arguments	;   up down dice random
	       part event clause 1)
	      (let ((args (->list (second clause))))
		(if (every #'(lambda (q)
			       (member q '(up down dice random)))
			   args)
		    (setf (epart-state-strum-direction state)
			  (cycle :of args))
		  (progn 
		    (cyco-warning
		     (sformat "Expected only: up, down, dice or random,  Encountered ~A" args)
		     (part-id part)
		     (sformat "Event  ~A" event)
		     (sformat "Clause ~A" clause))
		    (setf (epart-state-strum-direction state)
			  (cycle :of 'down))))))
	  state)

	 (process-strum-amp-scale	; :amp* s
	  (part event clause state)
	  (let ((scale (expect-float part event clause 1 :min 1/2 :max 2 :default 1.0)))
	    (setf (epart-state-strum-amp-scale state) scale)
	    state))

	 (process-strum-end-together	; :end-together no
	  (part event clause state)	; :end-together yes
	  (let ((arg (and (expect-n-arguments part event clause 1)
			  (second clause))))
	    (setf (epart-state-strum-end-together state)
		  (cond ((eq arg 'yes) t)
			((eq arg 'no) nil)
			(t (cyco-warning
			    (sformat "Expected either yes or no, encountered ~A" arg)
			    (part-id part)
	       		    (sformat "Event  ~A" event)
			    (sformat "Clause ~A" clause)
			    'no)))))
	  state)

	 (process-grace 		; :grace key delay
	  (part event clause state)
	  (if (and (expect-n-arguments part event clause 2)
		   (expect-metric-expression part event clause 2 0.001))
	      (let ((key (second clause))
		    (delay (metric-expression (third clause))))
		(setf (epart-state-grace-key state) key)
		(setf (epart-state-grace-delay state) delay)))
	  state)
		    
	 (process-grace-amp-scale	; :grace-amp* n
	  (part event clause state)
	  (let ((ascale (expect-normalized-value part event clause 1 0.25)))
	    (setf (epart-state-grace-amp-scale state) ascale))
	  state)

	 (process-grace-articulation	; :grace-dur ex
	  (part event clause state)
	  (let ((dur (expect-metric-expression part event clause 1 0.01)))
	    (setf (epart-state-grace-articulation state) dur))
	  state)
	 
	 (process-simple-dynamic	; :amp m
	  (part event clause state)	; :amp list --> cycle
	  (flet ((award ()
			(cyco-warning
			 (sformat "Expected dynamic values, encountered ~A" (cdr clause))
			 (part-id part)
			 (sformat "Event  ~A" event)
			 (sformat "Clause ~A" clause))))
	    (setf (epart-state-dynamic state)
		   (cycle :of (dynamic (if (expect-n-arguments part event clause 1)
	      				   (let ((args (->list (second clause))))
	      				     (if (every #'dynamic-p args)
	      					 args
	      				       (progn 
	      					 (award)
	      					 'mf)))
	      				 'mf))))
	    state))

	 (process-crescendo 		; :cres start end count
	  (part event clause state)
	  (let* ((start (dynamic (expect-dynamic-value part event clause 1 0.5)))
		 (end (dynamic (expect-dynamic-value part event clause 2 0.5)))
		 (count (expect-integer part event clause 3 :min 1 :default 1))
		 (diff (float (- (dynamic end)(dynamic start))))
		 (delta (/ diff count))
		 (dlist (range start end :by delta))
		 (pat (line :of dlist)))
	    (setf (epart-state-dynamic state) pat)
	    state))

	 (process-amp-blur		; :amp-blur s
	  (part event clause state)
	  (let ((blur (expect-float part event clause 1 :min 0.0 :max 1.0 :default 0.0)))
	    (setf (epart-state-dynamic-blur state) blur)
	    state))

	 (process-amp-range		; :amp-range min max
	  (part event clause state)
	  (let ((mn (expect-dynamic-value part event clause 1 0.0))
		(mx (expect-dynamic-value part event clause 2 1.0)))
	    (setf (epart-state-dynamic-min state) mn
		  (epart-state-dynamic-max state) mx)
	    state))

	 (process-articulation	       ; :dur metric-expression
	  (part event clause state)
	  (if (expect-n-arguments part event clause 1)
	      (let* ((arg (second clause))
		     (mxp (metric-expression-p arg)))
		(setf (epart-state-articulation state)
		      (cond ((and (numberp arg)(<= 0 arg))
			     arg)
			    (mxp arg)
			    (t
			     (cyco-warning "Expected positive float or metric-expression"
					   (sformat "Encounterd ~A" arg)
					   (part-id part)
					   (sformat "Event  ~A" event)
					   (sformat "Clause ~A" clause)
					   'q)))))
	    state))
			    
	    
	 (process-touch 		; :touch n
	  (part event clause state)
	  (let ((v (expect-normalized-value part event clause 1 0.0)))
	    (setf (epart-state-touch state) v)
	    state))

	 (process-controller-number	; :ctrl n
	  (part event clause state)
	  (let ((v (expect-integer part event clause 1 :min 0 :max 127 :default 1)))
	    (setf (epart-state-controller-number state) v)
	    state))

	 (process-controller-value	; :cc n
	  (part event clause state)
	  (let ((v (expect-normalized-value part event clause 1 0.0)))
	    (setf (epart-state-controller-value state) v)
	    state))

	 (process-bend			; :bend n
	  (part event clause state)
	  (let ((v (expect-signed-normalized-value part event clause 1 0.0)))
	    (setf (epart-state-bend state) v)
	    state))

	 (process-simple-program-change	; :program p
	  (part event clause state)
	  (if (expect-n-arguments part event clause 1)
	      (let ((v (second clause)))
		(setf (epart-state-program-number state)
		      (if (eq v 'default) :default v))))
	  state)

	 (process-compound-program-change ; :bank b program
	  (part event clause state)
	  (if (expect-n-arguments part event clause 2)
	      (let ((b (second clause))
		    (p (third clause)))
		(setf (epart-state-program-number state)
		      (if (eq p 'default) :default p))
		(setf (epart-state-program-bank state)
		      (if (eq b 'default) :default b))))
	  state)
	 
	 (dispatch-command
	  (event clause part state)
	  (let ((command (car clause))
		(result nil))
	    (setf result (cond ((eq command :time)
	    		       	(process-time part event clause state))
			       ((eq command :key)
				(process-key part event clause state))
			       ((eq command :chord)
				(process-chord-template part event clause state))
			       ((eq command :inversion)
				(process-chord-inversion part event clause state))
			       ((eq command :octave)
				(process-chord-octave part event clause state))
			       ((eq command :strum)
				(process-strum-delay part event clause state))
			       ((eq command :strum*)
				(process-strum-acceleration part event clause state))
			       ((eq command :direction)
				(process-strum-direction part event clause state))
			       ((eq command :amp*)
				(process-strum-amp-scale part event clause state))
			       ((eq command :end-together)
				(process-strum-end-together part event clause state))

			       ((eq command :grace)
				(process-grace part event clause state))

			       ((eq command :grace-amp*)
				(process-grace-amp-scale part event clause state))

			       ((eq command :grace-dur)
				(process-grace-articulation part event clause state))

			       
			       
			       ((eq command :amp)
				(process-simple-dynamic part event clause state))
			       ((eq command :cres)
				(process-crescendo part event clause state))
			       ((eq command :amp-blur)
				(process-amp-blur part event clause state))
			       ((eq command :amp-range)
				(process-amp-range part event clause state))
			       ((eq command :dur)
				(process-articulation part event clause state))
			       ((eq command :touch)
				(process-touch part event clause state))
			       ((eq command :ctrl)
				(process-controller-number part event clause state))
			       ((eq command :cc)
				(process-controller-value part event clause state))
			       ((eq command :bend)
				(process-bend part event clause state))
			       ((eq command :program)
				(process-simple-program-change part event clause state))
			       ((eq command :bank)
				(process-compound-program-change part event clause state))
	    		       (t
	    			(cyco-warning
	    			 (sformat "Invalid command: ~A" command)
	    			 (sformat "Section.Part  ~A.~A" (name (parent part))(name part))
	    			 (sformat "Event ~A" event))
	    			nil)))
	    result))

	 (switch-trace-mode
	  (clause)
	  (let ((mode (or (second clause) 'all)))
	    (setf *trace-epart-events* (or (eq mode 'events)(eq mode 'all)))
	    (setf *trace-epart-states* (or (eq mode 'state)(eq mode 'all)))))

	 ;; Returns true if state has non-nil key, grace, touch
	 ;; controller, bend, program or bank values.
	 ;; In other words, does state actually generate MIDI events?  
	 (real-event-p
	  (state)
	  (or (epart-state-key state)
	      (epart-state-grace-key state)
	      (epart-state-touch state)
	      (and (epart-state-controller-number state)
		   (epart-state-controller-value state))
	      
	      (epart-state-bend state)
	      (epart-state-program-number state)
	      (epart-state-program-bank state)))

	 (process-events
	  (part events)
	  (let ((acc '())
		(state (epart-state part)))
	    (reset state)
	    (block outer
	      (dolist (event events)
		(soft-reset state)
		(trace-event part event)
		(block inner
		  (dolist (clause (partition-list event))
		    (let ((command (car clause)))
		      (cond ((eq command :trace)
			     (switch-trace-mode clause))
			    ((eq command :break)
			     (return-from outer nil))
			    ((eq command :stop)
			     (return-from inner nil))
			    ((eq command :reset)
			     (reset state)
			     (return-from inner nil))
			    (t (dispatch-command event clause part state))))))
		(if (real-event-p state)
		    (let ((local-state (clone state)))
		      (setf (epart-state-source local-state) (->string event))
		      (trace-state local-state)
		      (push local-state acc)))))
	    (setf (epart-events part) acc))) )

  (defun make-epart (name instruments &key
			  section
			  (cuefn #'bar)
			  shift 
			  tempo unit bars beats subbeats
			  render-once
			  transposable
			  reversible
			  chord-model
			  remarks
			  events)
    (let* ((parent (or (validate-section name section)
		       (return-from make-epart nil))))
      (let* ((instrument-pattern (or (validate-instruments name instruments)
	   			     (return-from make-epart nil)))
	     (epart (make-instance 'epart
				   :properties +epart-properties+
				   :name name
				   :remarks (->string (or remarks ""))
				   :transient t)))
	(put epart :instruments instrument-pattern)
	(put epart :tempo tempo)
	(put epart :unit unit)
	(put epart :bars bars)
	(put epart :beats beats)
	(put epart :subbeats subbeats)
	(put epart :cue-function cuefn)
	(put epart :render-once render-once)
	(put epart :transposable transposable)
	(put epart :reversible reversible)
	(put epart :chord-model chord-model)
	(put epart :muted nil)
	(connect parent epart)
	(put epart :shift
      	      (funcall (property epart :cue-function) epart shift))
	(set-cyco-prompt)
	(setf (epart-events epart) (reverse (process-events epart (->list events))))
	epart)))
  ) ;; end labels

(setf (documentation 'make-epart 'function) +epart-documentation+)

(defmacro epart (name instruments &key 
		      section
		      shift
		      tempo unit bars beats subbeats
		      (cuefn #'bar)
		      render-once
		      transposable
		      reversible
		      chord-model
		      remarks
		      events)
 "Same as epart except binds the part to a symbol named name."
  `(progn
     (part-banner (name ,section) ',name)
     (let ((ep (make-epart ',name ,instruments
			   :section ,section
			   :shift ,shift
			   :tempo ,tempo
			   :unit ,unit
			   :bars ,bars
			   :beats ,beats
			   :subbeats ,subbeats
			   :cuefn ,cuefn
			   :render-once ,render-once
			   :transposable ,transposable
			   :reversible ,reversible
			   :chord-model ,chord-model
			   :remarks ,remarks
			   :events ,events)))
       (defparameter ,name ep)
       ep)))

(defmethod transpose ((p epart)(x t))
  (if (property p :transposable)
      (dolist (evn (epart-events p))
	(transpose evn x)))
  p)

(defmethod invert ((p epart)(pivot t))
  (if (property p :transposable)
      (dolist (evn (epart-events p))
	(invert evn pivot)))
  p)

(defmethod retrograde ((p epart))
  (if (property p :reversible)
      (let ((acc '()))
	(dolist (state (epart-events p))
	  (let ((kn (epart-state-key state)))
	    (if kn (push kn acc))))
	(dolist (state (epart-events p))
	  (let ((kn (epart-state-key state)))
	    (if kn (setf (epart-state-key state)(pop acc)))))))
  p)

(defmethod clone ((src epart) &key new-name new-parent)
  (let* ((frmt (or new-name "~A"))
	 (name (->symbol (sformat frmt (name src))))
	 (parent (or new-parent (parent src)))
	 (prt (make-epart name (clone (property src :instruments))
			  :section parent
			  :cuefn (property src :cue-function)
			  :transposable (property src :transposable)
			  :chord-model (property src :chord-model)
			  :remarks (remarks src)
			  :events '())))
    (copy-time-signature src prt)
    (setf (epart-state prt)(clone (epart-state src)))
    (let ((acc '()))
      (dolist (evn (epart-events src))
	(push (clone evn) acc))
      (setf (epart-events prt)(reverse acc)))
    (dolist (c (children src))
      (clone c :new-name new-name :new-parent prt))
    prt))

(defmethod dump-epart-events ((epart epart))
  (format t "Section.EPart  ~A.~A~%" (name (parent epart))(name epart))
  (let ((index 0))
    (dolist (s (epart-events epart))
      (format t "[~3D] -----------------------------------~%" index)
      (format t "~A~%" s)
      (setf index (1+ index)))))



