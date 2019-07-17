;;;; CYCO strummer
;;;;
;;;; 
;;;; An Strummer may generate note events and has a rich set of options for
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
;;;; :TRACE-ON
;;;; :TRACE-OFF
;;;;
;;;; :BREAK
;;;;     Skips evaluation of remaining events.
;;;;
;;;; :RESET
;;;;     Resets all parameters to initial condition.
;;;;
;;;; :TIME argument-list
;;;;     Specifies event time.  The accepted argument format is dependent on
;;;;     the part's cuing-function.  Time remains in effect for all future
;;;;     events until either:
;;;;     1) It is explicitly changed.
;;;;     3) On reset.
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
;;;;         down - play chord in note order
;;;;         up   - reverse note order
;;;;         dice - randomly select :up or :down
;;;;         random - select notes randomly.
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
;;;; :GRACE-AMP* n     0.25 <= n <= 4.0
;;;; :GRACE-DURATION ex
;;;; :GRACE-DELAY metric
;;;;
;;;; :DUR metric-expression|number
;;;;     Sets note duration.
;;;;     Duration remains in effect until explicitly changed.
;;;;     If duration is a non-negative number it is used as the absolute
;;;;     note duration, unless the instruments articulation-map changes it.
;;;;     If the duration is a metric-expression, it's value is scaled by 
;;;;     the current tempo.
;;;;
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
;;;; :AMP-LIMITS min max
;;;;    Sets minimum and maximum dynamic values. Limiting is applied after amp+,
;;;;    amp*, amp-blur and any strum scaling.  Both min and max are dynamic
;;;;    values and remain in effect until explicitly changed.
;;;;
;;;; -------- 'real' events
;;;;
;;;; :KEY k
;;;;     Creates a note event.
;;;;     The k argument is typically a keynumber, but it need not be so long as
;;;;     the instrument's keynumber-map is able to convert it to a keynumber.
;;;; 
;;;; :GRACE key
;;;;    Play grace note
;;;;    key    - key number
;;;;    Grave notes ignore chord
;;;;
;;;; :BEND b
;;;;    Creates a single MIDI bend event 
;;;;    -1.0 <= b <= +1.0
;;;; 
;;;; :CC ctrl n
;;;;   Creates single MIDI controller event 
;;;;   controller number established by CTRL.
;;;;
;;;;   0 <= ctrl < 128
;;;;   0.0 <= n <= 1.0
;;;; 
;;;; :PROGRAM p
;;;;    Creates simple program-change events
;;;;    The expected format for p is dependent on the instrument's
;;;;    program-map functions.
;;;; 
;;;;    A value of default should cause the instruments to generate their
;;;;    default programs.   Numeric p values are typically interpreted as
;;;;    explicit MIDI program numbers."	  
;;;;
;;;; :BANK b p
;;;;    Creates bank and program-change events.
;;;;    The expected format for bank b and program p is dependent
;;;;    on the instruments program-map functions.
;;;; 
;;;;    A value of default should cause the instruments to generate their
;;;;    default bank/programs.   Numeric p values are typically interpreted as
;;;;    explicit MIDI program numbers."	  
;;;; 

(constant +strummer-properties+
	  (append +part-properties+
		  '(:shift
		    :render-once)))

(defclass strummer (part)
  ((events				
    :type list   ; list of states
    :accessor strummer-events
    :initform '())))

(defmethod strummer-p ((obj strummer)) t)

(global *trace-strummer-events* nil)
(global *trace-strummer-states* nil)

(labels ((trace-event (part event)
		      (if *trace-strummer-events*
			  (let ((frmt "TRACE: ~A.~A event ~A~%"))
			    (format t "~A~%" +banner-bar2+)
			    (format t frmt (name (parent part))(name part) event))))
	 
	 (trace-state (state)
		      (if *trace-strummer-states*
			  (format t "~A~%" state)))
	 
	 (part-id (part)
		  (sformat "Section.Part -->  ~A.~A" (name (parent part))(name part)))
	 
	 ;; Test that clause has between n and m arguments.
	 ;; Returns nil on error.
	 ;;
	 (expect-n-arguments (part event clause n &optional m)
			     (let* ((count (1- (length clause)))
				    (mn n)
				    (mx (or m n)))
			       (if (not (and (<= mn count)(<= count mx)))
				   (cyco-warning
				    (sformat "Expected between ~A and ~A arguments, encountered ~A" mn mx count)
				    (part-id part)
				    (sformat "Event ~A" event)
				    (sformat "Clause ~A" clause))
				 t)))

	 (expect-0-arguments (part event clause)
			     (expect-n-arguments part event clause 0))

	 (expect-1-argument (part event clause)
			    (expect-n-arguments part event clause 1))
	 
	 ;; Test that nth argument of clause passes test.
	 ;; On error print warning and return default.
	 ;; On non-error return argument.
	 (expect-nth-argument (part event clause position label test default)
			      (let ((nth-argument (nth position clause)))
				(if (not (funcall test nth-argument))
				    (progn
				      (cyco-warning
				       (sformat "Expected ~A, Encountered ~A" label nth-argument)
				       (part-id part)
				       (sformat "Event ~A" event)
				       (sformat "Clause ~A" clause)
				       (sformat "Using default ~A" default))
				      default)
				  nth-argument)))
		    
	 (expect-metric-expression (part event clause position default)
				   (expect-nth-argument
				    part event clause position "metric-expression"
				    #'metric-expression-p default))
	 
	 (expect-integer (part event clause position &key (min -1e6)(max 1e6)(default 0))
			 (truncate (expect-nth-argument
				    part event clause position
				    (sformat "integer in range (~A,~A)" min max)
				    #'(lambda (q)(and (integerp q)(<= min q)(<= q max)))
				    default)))
	 
	 (expect-float (part event clause position &key (min -1e6)(max +1e6)(default 0.0))
		       (float (expect-nth-argument
			       part event clause position
			       (sformat "float in range (~A,~A)" min max)
			       #'(lambda (q)(and (numberp q)(<= min q)(<= q max)))
			       default)))

	 (expect-dynamic (part event clause position &key (default 0.5))
			 (let ((args (->list (nth position clause))))
			   (if (not (every #'dynamic-p args))
			       (progn
				 (cyco-warning
				  (sformat "Expected dynamic values, encountered ~A" args)
				  (part-id part)
				  (sformat "Event  ~A" event)
				  (sformat "Clause ~A" clause)
				  (sformat "Using default ~A" default))
				 (if default (list default) nil))
			     args)))
	 
	 ;; :reset
	 ;; Set all state parameters to default values
	 (process-reset (part event clause state)
			(if (expect-0-arguments part event clause)
			    (reset state)))
	 
	 ;; :time (time-specification)
	 ;; tspec must be in form accept by cue-function
	 (process-time (part event clause state)
		       (if (expect-1-argument part event clause) 
			   (let* ((cuefn (property part :cue-function))
				  (args (second clause))
				  (time (funcall cuefn part args)))
			     (setf (strummer-state-time state)(float time)))))

	 ;; :chord name
	 ;; :chord template
	 (process-chord (part event clause state)
			(if (expect-1-argument part event clause) 
			    (let ((chord-type (second clause))
				  (chord-model (property part :chord-model)))
			      (cond ((listp chord-type)
				     nil)
				    ((not (defines-chord-p chord-model chord-type))
				     (progn
				       (cyco-warning
					(sformat "CHORD-MODEL ~A does not define chord ~A"
						 (name chord-model) chord-type)
					(part-id part)
					(sformat "Event ~A" event)
					"Using default (0)")
				       (setf chord-type '(0))))
				    (t nil))
			      (setf (strummer-state-chord-type state) chord-type))))

	 ;; :inversion n     -12 <= n <= +12
	 (process-chord-inversion (part event clause state)
				  (if (expect-1-argument part event clause) 
				      (let ((n (expect-integer part event clause 1 :min -12 :max +12 :default 0)))
					(setf (strummer-state-chord-inversion state) n))))
	 
	 ;; :octave n   -3 <= n <= 3
	 (process-chord-octave (part event clause state)
			       (if (expect-1-argument part event clause) 
				   (let ((n (expect-integer part event clause 1 :min -3 :max +3 :default 0)))
				     (setf (strummer-state-chord-octave state) n))))

	 ;; strum delay
	 ;; where delay is metric-expression
	 (process-strum-delay (part event clause state)
			      (if (expect-1-argument part event clause) 
				  (let* ((metric-expression (expect-metric-expression part event clause 1 0.0))
					 (delay (if (numberp metric-expression)
						    (abs (float metric-expression))
						  (* (metric-expression metric-expression)
						     (/ 60.0 (tempo part))))))
				    (setf (strummer-state-strum-delay state) delay))))

	 ;; :strum* n   float 0.125 <= n <= 8.00
	 (process-strum-acceleration (part event clause state)
				     (if (expect-1-argument part event clause) 
					 (let ((scale (expect-float part event clause 1 :min 0.125 :max 8 :default 1)))
					   (setf (strummer-state-strum-acceleration state) scale))))

	 ;; :direction d
	 ;; :direction (list...)
	 ;;    valid directions down, up, coin, random
	 (process-strum-direction (part event clause state)
				  (if (expect-1-argument part event clause) 
				      (let ((pattern (->list (second clause))))
					(if (every #'(lambda (q)(member q '(up down coin random))) pattern)
					    (setf (strummer-state-strum-direction state)(cycle :of pattern))
					  (progn 
					    (cyco-warning
					     (sformat "Expected strum direction or list of directions, encountered ~A" pattern)
					     "Valid directions are: up down coin & random"
					     (part-id part)
					     (sformat "Event  ~A" event)
					     (sformat "Clause ~A" clause)
					     "Using default: down")
					    (setf (strummer-state-strum-direction state)(line :of 'down)))))))

	 ;; :amp* n   0.25 <= n < 4
	 (process-strum-amp-scale (part event clause state)
				  (if (expect-1-argument part event clause)
				      (let ((scale (expect-float part event clause 1 :min 0.25 :max 4 :default 1)))
					(setf (strummer-state-strum-amp-scale state) scale))))

	 ;; :end-together no|yes
	 (process-end-together (part event clause state)
			       (if (expect-1-argument part event clause)
				   (let ((argument (second clause)))
				     (setf (strummer-state-strum-end-together state)
					   (cond ((eq argument 'no) nil)
						 ((eq argument 'yes) t)
						 (t (cyco-warning
						     (sformat "Expected either yes or no, encountered ~A" argument)
						     (part-id part)
						     (sformat "Event  ~A" event)
						     (sformat "Clause ~A" clause)
						     "Using default: no")
						    nil))))))

	 ;; :grace-amp* n   0.0 <= n <= 4
	 (process-grace-amp-scale (part event clause state)
				  (if (expect-1-argument part event clause)
				      (let ((scale (expect-float part event clause 1 :min 0.0 :max 4 :default 0.5)))
					(setf (strummer-state-grace-amp-scale state) scale))))

	 ;; :grace-duration metric
	 ;; :grace-duration number
	 (process-grace-articulation (part event clause state)
				     (if (expect-1-argument part event clause)
					 (let* ((metric-expression (expect-metric-expression part event clause 1 's))
						(delay (if (numberp metric-expression)
							   (abs (float metric-expression))
							 (* (metric-expression metric-expression)
							    (/ 60.0 (tempo part))))))
					   (setf (strummer-state-grace-articulation state) delay))))

	 ;; :grace-delay metric
	 ;; :grace-delay metric
	  (process-grace-delay (part event clause state)
			       (if (expect-1-argument part event clause)
				   (let* ((metric-expression (expect-metric-expression part event clause 1 's))
					  (delay (if (numberp metric-expression)
						     (abs (float metric-expression))
						   (* (metric-expression metric-expression)
						      (/ 60.0 (tempo part))))))
				     (setf (strummer-state-grace-delay state) delay))))
	 
	 ;; :dur metric
	 ;; :dur number
	 (process-note-articulation (part event clause state)
				    (if (expect-1-argument part event clause)
					(let* ((metric-expression (expect-metric-expression part event clause 1 's))
					       (delay (if (numberp metric-expression)
							  (abs (float metric-expression))
							(* (metric-expression metric-expression)
							   (/ 60.0 (tempo part))))))
					  (setf (strummer-state-articulation state) delay))))

	 ;; :amp dynamic
	 ;; :amp list (converted to cycle)
	 (process-simple-dynamic (part event clause state)
				 (if (expect-1-argument part event clause)
				     (let ((dynamics-list (expect-dynamic part event clause 1)))
				       (setf (strummer-state-dynamic state)(cycle :of dynamics-list)))))

	 ;; :cres start end count
	 (process-crescendo (part event clause state)
			    (if (and (expect-n-arguments part event clause 3)
				     (expect-dynamic part event clause 1 :default nil)
				     (expect-dynamic part event clause 2 :default nil)) 
				(let* ((start (dynamic (second clause)))
				       (end (dynamic (third clause)))
				       (count (expect-integer part event clause 3 :min 1 :max 128 :default 8))
				       (diff (float (- (dynamic end)(dynamic start))))
				       (delta (/ diff count))
				       (pattern (line :of (range start end :by delta))))
				  (setf (strummer-state-dynamic state) pattern))))

	 ;; :amp-blur n   0.0 <= n <= 1.0
	 (process-dynamic-blur (part event clause state)
			       (if (expect-1-argument part event clause)
				   (let ((blur-ratio (expect-float part event clause 1 :min 0 :max 1 :default 0)))
				     (setf (strummer-state-dynamic-blur state) blur-ratio))))
	 
	 ;; :amp-limits min max
	 (process-dynamic-limits (part event clause state)
				 (if (and (expect-n-arguments part event clause 2)
					  (expect-dynamic part event clause 1 :default nil)
					  (expect-dynamic part event clause 2 :default nil))
				     (let ((value-1 (dynamic (second clause)))
					   (value-2 (dynamic (third clause))))
				       (setf (strummer-state-dynamic-min state)(min value-1 value-2))
				       (setf (strummer-state-dynamic-max state)(max value-1 value-2)))))

	 ;; :key n
	 ;;    Can not check validity of keynumber prior to
	 ;;    application of instruments' keynumber-map.
	 (process-key (part event clause state)
		      (if (expect-1-argument part event clause)
			  (setf (strummer-state-key state)(second clause))))

	 ;; :grace n
	 ;;    Can not check validity of keynumber prior to
	 ;;    application of instruments' keynumber-map.
	 (process-grace-note (part event clause state)
			     (if (expect-1-argument part event clause)
				 (setf (strummer-state-grace-key state)(second clause))))

	 ;; :bend n    -1.0 <= n <= +1
	 (process-bend (part event clause state)
		       (if (expect-1-argument part event clause)
			   (let ((bend-amount (expect-float part event clause 1 :min -1.0 :max +1.0 :default 0.0)))
			     (setf (strummer-state-bend state) bend-amount))))

	 ;; :cc ctrl n    ctrl {0,1,2,3,...,127}
	 ;;               0.0 <= n <= 1.0
	 (process-controller (part event clause state)
			     (if (and (expect-n-arguments part event clause 2)
				      (expect-integer part event clause 1 :min 0 :max 127 :default nil)
				      (expect-float part event clause 2 :min 0.0 :max 1.0 :default nil))
				 (let ((controller-number (second clause))
				       (value (third clause)))
				   (setf (strummer-state-controller-number state) controller-number
					 (strummer-state-controller-value state) value))))
	      
	 ;; :pogram argument
	 ;;         Argument is either a MIDI program number or the keyword :default.
	 ;;         For :default the program number is derived from the instrument.
	 ;;
	 (process-simple-program (part event clause state)
				 (if (expect-1-argument part event clause)
				     (let ((program-number (second clause)))
				       (setf (strummer-state-program-number state)
					     (if (eq program-number 'default) :default program-number)))))
				 

	 ;; :bank bank program
	 ;;       Eith bank or program may be the keyword :default
	 (process-bank-and-program (part event clause state)
				   (if (expect-n-arguments part event clause 2)
				       (let ((bank (second clause))
					     (program (third clause)))
					 (setf (strummer-state-program-bank state)
					       (if (eq bank 'default) :default bank)
					       (strummer-state-program-number state)
					       (if (eq program 'default) :default program)))))
	 
	 ;; Conditional cons if object is non-nil
	 (cons? (list object)
		(if object
		    (cons object list)
		  list))

	 ;; Returns true if state has non-nil key, grace,
	 ;; controller, bend, program or bank values.
	 ;; In other words, does state actually generate MIDI events?
	 (real-event-p (state)
		       (or (strummer-state-key state)
			   (strummer-state-grace-key state)
			   (and (strummer-state-controller-number state)
				(strummer-state-controller-value state))
			   (strummer-state-bend state)
			   (strummer-state-program-number state)
			   (strummer-state-program-bank state)))

	 ;; Returns new state if clause produces 'real' event.
	 ;; Returns nil otherwise
	 ;; State may be changed by side-effect
	 (dispatch-event (part state event clause)
			 (let ((command (car clause)))
			   (cond ((eq command :trace-on)
				  (setf *trace-strummer-events* t
					*trace-strummer-states* t))
				 ((eq command :trace-off)
				  (setf *trace-strummer-events* nil
					*trace-strummer-states* nil))
				 ((eq command :reset)
				  (process-reset part event clause state))
				 ((eq command :time)
				  (process-time part event clause state))
				 ((eq command :chord)
				  (process-chord part event clause state))
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
				  (process-end-together part event clause state))
				 ((eq command :grace-amp*)
				  (process-grace-amp-scale part event clause state))
				 ((eq command :grace-duration)
				  (process-grace-articulation part event clause state))
				 ((eq command :grace-delay)
				  (process-grace-delay part event clause state))
				 ((eq command :dur)
				  (process-note-articulation part event clause state))
				 ((eq command :amp)
				  (process-simple-dynamic part event clause state))
				 ((eq command :cres)
				  (process-crescendo part event clause state))
				 ((eq command :amp-blur)
				  (process-dynamic-blur part event clause state))
				 ((eq command :amp-limits)
				  (process-dynamic-limits part event clause state))
				 ((eq command :key)
				  (process-key part event clause state))
				 ((eq command :grace)
				  (process-grace-note part event clause state))
				 ((eq command :bend)
				  (process-bend part event clause state))
				 ((eq command :cc)
				  (process-controller part event clause state))
				 ((eq command :program)
				  (process-simple-program part event clause state))
				 ((eq command :bank)
				  (process-bank-and-program part event clause state))
				 (t
				  (cyco-error
				   (sformat "Invalid Strummer ~A event: ~A"
					    (name part) command)))))) 

	 (process-strummer-events (strummer event-list)
				  (let ((acc '())
					(state (make-strummer-state)))
				    (reset state)
				    (block event-loop
				      (dolist (event event-list)
					(trace-event strummer event)
					(dolist (clause (partition-list event))
					  (let ((command (car clause)))
					    (cond ((eq command :break)
						   (return-from event-loop))
						  (t (dispatch-event strummer state event clause)))))
					(trace-state state)
					(if (real-event-p state)
					    (progn 
					      (push (clone state) acc)
					      (soft-reset state)))))
				    (setf (strummer-events strummer)(reverse acc))))
	 
	 (validate-section (part-name section)
			   (cond ((section-p section)
				  section)
				 ((and section (not (section-p section)))
				  (cyco-type-error 'make-strummer '(section nil) section)
				  nil)
				 ((not (project-p *project*))
				  (cyco-composition-error
				   'make-strummer
				   (sformat "No default project while creating strummer ~A" part-name))
				  nil)
				 (t (property *project* :current-section))))

	 (validate-instrument (part-name instrument)
			      (if (not (instrument-p instrument))
				  (progn 
				    (cyco-type-error 'make-strummer 'instrument instrument
						     (sformat "Expected single instrument for Strummer: ~A" part-name)
						     "Using *ROOT-INSTRUMENT*")
				    *root-instrument*)
				instrument)) )

  (defun make-strummer (name instrument &key
			     section
			     cuefn
			     shift 
			     tempo unit bars beats subbeats
			     render-once
			     transposable
			     reversible
			     chord-model
			     remarks
			     events)
    (let* ((parent (or (validate-section name section)
		       (return-from make-strummer nil))))
      (let* ((new-strummer (make-instance 'strummer
				      :properties +strummer-properties+
				      :name name
				      :remarks (->string (or remarks ""))
				      :transient t)))
	(put new-strummer :instruments (validate-instrument name instrument))
	(put new-strummer :tempo tempo)
	(put new-strummer :unit unit)
	(put new-strummer :bars bars)
	(put new-strummer :beats beats)
	(put new-strummer :subbeats subbeats)
	(put new-strummer :cue-function cuefn)
	(put new-strummer :render-once render-once)
	(put new-strummer :transposable transposable)
	(put new-strummer :reversible reversible)
	(put new-strummer :chord-model chord-model)
	(put new-strummer :muted nil)
	(put new-strummer :shift (or shift 0.0))
	(connect parent new-strummer)
	(set-cyco-prompt)
	(setf (strummer-events new-strummer)
	      (process-strummer-events new-strummer (->list events)))
	new-strummer))) ) 

(setf (documentation 'make-strummer 'function) +strummer-documentation+)

(defmacro strummer (name instrument &key 
			 section
			 shift
			 tempo unit bars beats subbeats
			 cuefn
			 render-once
			 transposable
			 reversible
			 chord-model
			 remarks
			 events)
  "Same as strummer except binds the part to a symbol named name."
  `(progn
     (part-banner (name ,section) ',name)
     (let ((new-strummer (make-strummer ',name ,instrument
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
       (defparameter ,name new-strummer)
       new-strummer)))

(defmethod transpose ((strummer strummer)(x t))
  (if (property strummer :transposable)
      (dolist (evn (strummer-events strummer))
	(transpose evn x)))
  strummer)

(defmethod invert ((strummer strummer)(pivot t))
  (if (and pivot (property strummer :transposable))
      (dolist (evn (strummer-events strummer))
	(invert evn pivot)))
  strummer)

(defmethod retrograde ((strummer strummer))
  (if (property strummer :reversible)
      (let ((acc '()))
	(dolist (state (strummer-events strummer))
	  (let ((kn (strummer-state-key state)))
	    (if kn (push kn acc))))
	(dolist (state (strummer-events strummer))
	  (let ((kn (strummer-state-key state)))
	    (if kn (setf (strummer-state-key state)(pop acc)))))))
  strummer)

(defmethod clone ((source strummer) &key new-name new-parent)
  (let* ((name (->symbol (sformat (or new-name "~A") (name source))))
	 (parent (or new-parent (parent source)))
	 (new-strummer (make-strummer name (clone (property source :instruments))
				      :section parent
				      :cuefn (property source :cue-function)
				      :transposable (property source :transposable)
				      :chord-model (property source :chord-model)
				      :remarks (remarks source)
				      :events '())))
    (copy-time-signature source new-strummer)
    (let ((acc '()))
      (dolist (evn (strummer-events source))
	(push (clone evn) acc))
      (setf (strummer-events new-strummer)(reverse acc)))
    (dolist (c (children source))
      (clone c :new-name new-name :new-parent new-strummer))
    new-strummer))

(defmethod dump-strummer-events ((strummer strummer))
  (format t "Section.Strummer  ~A.~A~%" (name (parent strummer))(name strummer))
  (let ((index 0))
    (dolist (strummer-state (strummer-events strummer))
      (format t "[~3D] -----------------------------------~%" index)
      (format t "~A~%" strummer-state)
      (setf index (1+ index)))))



