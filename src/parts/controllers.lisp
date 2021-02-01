;;;; CYCO parts controllers.lisp
;;;;
;;;; Defines the CONTROLLERS and BENDER classes.
;;;;
;;;; CONTROLLERS is a part for generating MIDI control-change and
;;;; channel-pressure events.
;;;; 
;;;; BENDER is a part for generating MIDI pitch-bend events.
;;;;

(in-package :cyco-part)

(constant +controllers-properties+
	  (append +part-properties+ '(:no-thin)))


(defclass controllers (part)
  ((states
    :type list
    :accessor controllers-states
    :initform '())))

(defgeneric controllers-p (item))
(defmethod controllers-p ((item t)) nil)
(defmethod controllers-p ((item controllers)) t)


(defclass bender (part)
  ((states
    :type list
    :accessor bender-states
    :initform '())))

(defgeneric bender-p (item))
(defmethod bender-p ((item t)) nil)
(defmethod bender-p ((item bender)) t)


(labels ((real-event-p 
	  (state)
	  (or (controllers-state-curve state)
	      (controllers-state-single-event state)))

	 ;; clause ~ (:time start end interval)
	 ;;   Where start and end are time-specification as required by
	 ;;   by the part's cue function, and interval is a metric-expression.
	 ;;
	 ;;   Special value '* indicates to reuse the current value.
	 ;;   Special start value '< sets start time to current end time.
	 ;;
	 (process-times 
	  (part state clause)
	  (let* ((cuefn (property part :cue-function))
		 (shuffle (property part :shuffle-function))
		 (start-spec (second clause))
		 (end-spec (third clause))
		 (interval-spec (fourth clause))
		 (start-time (cond ((or (eq start-spec '*)(null start-spec))
				    (controllers-state-start-time state))
				   ((eq start-spec '<)
				    (controllers-state-end-time state))
				   (t
				    (+ (funcall cuefn part start-spec)
				       (funcall shuffle start-spec)))))
		 (end-time (if (or (eq end-spec '*)(null end-spec))
			       (controllers-state-end-time state)
			     (+ (funcall cuefn part end-spec)
				(funcall shuffle end-spec))))
		 (time-interval (if (or (eq interval-spec '*)(null interval-spec))
				    (controllers-state-time-interval state)
				  (scale-time-parameter interval-spec part))))
	    (if (> start-time end-time)
		(let ((temp start-time))
		  (setf start-time end-time)
		  (setf end-time temp)))
	    (setf (controllers-state-start-time state) start-time)
	    (setf (controllers-state-end-time state) end-time)
	    (setf (controllers-state-time-interval state) time-interval)
	    t))

	 ;; clause ~ (:value start end)
	 ;;   Where values are integer MIDI data,  0 <= n < 128.
	 ;;
	 (process-controller-values 
	  (part state clause)
	  (let* ((start-spec (second clause))
		 (end-spec (third clause))
		 (start-value (cond ((or (eq start-spec '*)(null start-spec))
				     (controllers-state-start-value state))
				    ((eq start-spec '<)
				     (controllers-state-end-value state))
				    (t start-spec)))
		 (end-value (if (or (eq end-spec '*)(null end-spec))
				(controllers-state-end-value state)
			      end-spec)))
	    (if (and (numberp start-value)(numberp end-value))
		(setf (controllers-state-start-value state)
		      (limit (round start-value) 0 127)
		      (controllers-state-end-value state)
		      (limit (round end-value) 0 127))
	      (cyco-composition-error 'make-controllers
				      (sformat "part name was ~A" (name part))
				      (sformat "events clause was ~A" clause)
				      "Illegal controller value"))))

	 ;; clause ~ (:value start end)
	 ;;   Where start and end are signed normalized floats, -1.0 <= n <= +1.0
	 ;;
	 (process-bender-values
	  (part state clause)
	  (let* ((start-spec (second clause))
		 (end-spec (third clause))
		 (start-value (cond ((or (eq start-spec '*)(null start-spec))
				     (controllers-state-start-value state))
				    ((eq start-spec '<)
				     (controllers-state-end-value state))
				    (t start-spec)))
		 (end-value (if (or (eq end-spec '*)(null end-spec))
				(controllers-state-end-value state)
			      end-spec)))
	    (if (and (numberp start-value)(numberp end-value))
		(setf (controllers-state-start-value state)
		      (float (limit start-value -1 1))
		      (controllers-state-end-value state)
		      (float (limit end-value -1 1)))
	      (cyco-composition-error 'make-bender
				      (sformat "part name was ~A" (name part))
				      (sformat "events clause was ~A" clause)
				      "Illegal bender value"))))

	
	 ;; Converts controller-number specification to integer.
	 ;;    Where ctrl is integer MIDI controller number, 0 <= ctrl < 128,
	 ;;    a symbolic controller name,
	 ;;    or the special case symbol 'pressure.
	 ;;
	 (convert-controller-number
	  (ctrl)
	  (cond ((numberp ctrl)
		 (limit (round ctrl) 0 127))
		;; Not sure why this comparison fails.
		;; Using string comparison as workaround.
		;; ((eq ctrl 'pressure)
		;;  'pressure)
		((string= (->string ctrl) "PRESSURE")
		 'pressure)
		(t (get-controller-number ctrl :default 1))))
	 
	 (process-controller-type
	  (state clause)
	  (let ((ctrl (second clause)))
	    (setf (controllers-state-controller state)
		  (convert-controller-number ctrl))))

	 ;; clause ~ (:cycles n)
	 ;;    Where n is positive wave-cycle count.
	 ;;
	 (process-cycle-count 
	  (state clause)
	  (setf (controllers-state-cycles state)
		(abs (round (or (second clause) 1))))
	  t)

	 ;; clause ~ (:phase deg)
	 ;;    Where deg is phase shift in degrees.
	 ;;
	 (process-phase
	  (state clause)
	  (setf (controllers-state-phase state)
		(rem (round (or (second clause) 0)) 360))
	  t)

	 ;; clause ~ (:width w)
	 ;;    Where w is pulse width.  0.0 < w < 1.0
	 ;;
	 (process-pulse-width 
	  (state clause)
	  (setf (controllers-state-width state)
		(limit (or (second clause) 0.5) 0.0 1.0))
	  t)

	 ;; clause ~ (:cc time controller-number value)
	 ;;
	 (add-single-controller-event
	  (part state clause)
	  (let* ((cuefn (property part :cue-function))
		 (shuffle (property part :shuffle-function))
		 (time-spec (second clause))
		 (ctrl-spec (or (third clause) 1))
		 (value-spec (or (fourth clause) 0))
		 (time (+ (funcall cuefn part time-spec)
			  (funcall shuffle time-spec)))
		 (controller-number (convert-controller-number ctrl-spec))
		 (value (limit (round value-spec) 0 127)))
	    (setf (controllers-state-single-event state)
		  (list time controller-number value))))

	 ;; clause ~ (:bend time value)
	 ;;
	 (add-single-bend-event
	  (part state clause)
	  (let* ((cuefn (property part :cue-function))
		 (shuffle (property part :shuffle-function))
		 (time-spec (second clause))
		 (value-spec (third clause))
		 (time (+ (funcall cuefn part time-spec)
			  (funcall shuffle time-spec)))
		 (value (float (limit value-spec -1 +1))))
	    (setf (controllers-state-single-event state)
		  (list time :bend value))))
	 
	 (add-curve-event
	  (curve-type state)
	  (setf (controllers-state-curve state) curve-type)
	  t)

	 ;; Handles clauses common to both CONTROLLERS and BENDER.
	 ;;
	 (dispatch-common-clauses
	  (part state clause)
	  (let ((command (car clause)))
	     (cond
	      ((eq command :reset)(progn (reset state) t))
	      ((eq command :time) (process-times part state clause))
	      ((eq command :cycles) (process-cycle-count state clause))
	      ((eq command :phase) (process-phase state clause))
	      ((eq command :width) (process-pulse-width state clause))
	      ((eq command :ramp) (add-curve-event :ramp state))
	      ((eq command :saw) (add-curve-event :saw state))
	      ((eq command :tri) (add-curve-event :tri state))
	      ((eq command :pulse) (add-curve-event :pulse state))
	      (t nil))))
	 
	 (dispatch-controllers-clauses
	  (part state clause)
	  (let ((command (car clause)))
	    (or (dispatch-common-clauses part state clause)
		(cond
		 ((eq command :value)
		  (process-controller-values part state clause))
		 ((eq command :ctrl)
		  (process-controller-type state clause))
		 ((eq command :cc)
		  (add-single-controller-event part state clause))
		 (t (cyco-composition-error
		     'make-controllers
		     (sformat "Invalid controllers clause ~A" clause)))))))

	 (dispatch-bender-clauses
	  (part state clause)
	  (let ((command (car clause)))
	    (or (dispatch-common-clauses part state clause)
		(cond ((eq command :value)(process-bender-values part state clause))
		      ((eq command :bend)
		       (add-single-bend-event part state clause))
		      (t (cyco-composition-error
		     'make-bender
		     (sformat "Invalid controllers clause ~A" clause)))))))
	 
	 (process-controllers-events 
	  (part event-list)
	  (let* ((acc '())
		 (state (make-controllers-state)))
	    (reset state)
	    (dolist (event event-list)
	      (soft-reset state)
	      (setf (controllers-state-source state) event)
	      (dolist (clause (partition-list event))
		(dispatch-controllers-clauses part state clause)
		(if (real-event-p state)
		    (progn 
		      (push (clone state) acc)
		      (soft-reset state)))))
	    (reverse acc)))

	 (process-bender-events 
	  (part event-list)
	  (let* ((acc '())
		 (state (make-controllers-state)))
	    (reset state)
	    (dolist (event event-list)
	      (soft-reset state)
	      (setf (controllers-state-source state) event)
	      (dolist (clause (partition-list event))
		(dispatch-bender-clauses part state clause)
		(if (real-event-p state)
		    (progn
		      (push (clone state) acc)
		      (soft-reset state)))))
	    (reverse acc))) )
			   
  (defun make-controllers (name instruments &key
			        section
				cuefn
				shuffle
				shift
				tempo unit bars beats subbeats
				render-once
				remarks
				no-thin
				events)
    (let* ((part-name (->cyco-symbol name))
	   (parent (validate-section part-name section))
	   (part (make-instance 'controllers
				:properties +controllers-properties+
				:name part-name
				:remarks (->string (or remarks ""))
				:transient t)))
      (connect parent part)
      (put part :instruments (->list instruments))
      (put part :cue-function cuefn)
      (put part :shuffle-function shuffle)
      (put part :shift (scale-time-parameter (or shift 0) part))
      (put part :tempo tempo)
      (put part :unit unit)
      (put part :bars bars)
      (put part :beats beats)
      (put part :subbeats subbeats)
      (init-time-signature part)
      (put part :render-once render-once)
      (put part :transposable nil)
      (put part :reversible nil)
      (put part :no-thin no-thin)
      (setf (controllers-states part)
	    (process-controllers-events part (->list events)))
      (reset part)
      part))

  (defun make-bender (name instruments &key
			        section
				cuefn
				shuffle
				shift
				tempo unit bars beats subbeats
				render-once
				remarks
				no-thin
				events)
    (let* ((part-name (->cyco-symbol name))
	   (parent (validate-section part-name section))
	   (part (make-instance 'bender
				:properties +controllers-properties+
				:name part-name
				:remarks (->string (or remarks ""))
				:transient t)))
      (connect parent part)
      (put part :instruments (->list instruments))
      (put part :cue-function cuefn)
      (put part :shuffle-function shuffle)
      (put part :shift (scale-time-parameter (or shift 0) part))
      (put part :tempo tempo)
      (put part :unit unit)
      (put part :bars bars)
      (put part :beats beats)
      (put part :subbeats subbeats)
      (init-time-signature part)
      (put part :render-once render-once)
      (put part :transposable nil)
      (put part :reversible nil)
      (put part :no-thin no-thin)
      (setf (bender-states part)
	    (process-bender-events part (->list events)))
      (reset part)
      part)) )

	   
 (defmacro controllers (name instruments &key
			     section
			     cuefn
			     shuffle
			     shift
			     tempo unit bars beats subbeats
			     render-once
			     no-thin
			     remarks
			     events)
   `(progn
      (part-banner (name ,section) ',name)
      (let ((part (make-controllers ',name ,instruments
				   :section ,section
				   :cuefn ,cuefn
				   :shuffle ,shuffle
				   :shift ,shift
				   :tempo  ,tempo 
				   :unit  ,unit 
				   :bars  ,bars 
				   :beats  ,beats 
				   :subbeats ,subbeats
				   :render-once ,render-once
				   :no-thin ,no-thin
				   :remarks ,remarks
				   :events ,events)))
	(defparameter ,name part)
	part)))


(defmacro bender (name instruments &key
		       section
		       cuefn
		       shuffle
		       shift
		       tempo unit bars beats subbeats
		       render-once
		       remarks
		       no-thin
		       events)
  `(progn
     (part-banner (name ,section) ',name)
     (let ((part (make-bender ',name ,instruments
			      :section ,section
			      :cuefn ,cuefn
			      :shuffle ,shuffle
			      :shift ,shift
			      :tempo  ,tempo 
			      :unit  ,unit 
			      :bars  ,bars 
			      :beats  ,beats 
			      :subbeats ,subbeats
			      :render-once ,render-once
			      :no-thin ,no-thin
			      :remarks ,remarks
			      :events ,events)))
       (defparameter ,name part)
       part)))

   
(defmethod clone ((mother controllers) &key new-name new-parent)
  (let* ((frmt (or new-name "~A"))
	 (name (->symbol (sformat frmt (name mother))))
	 (parent (or new-parent (parent mother)))
	 (daughter (make-controllers name
				     (property mother :instruments)
				     :section parent
				     :no-thin (property mother :no-thin)
				     :remarks (remarks mother))))
    (copy-part-properties mother daughter)
    (setf (controllers-states daughter)(clone (controllers-states mother)))
    (copy-time-signature mother daughter)
    daughter))

(defmethod clone ((mother bender) &key new-name new-parent)
  (let* ((frmt (or new-name "~A"))
	 (name (->symbol (sformat frmt (name mother))))
	 (parent (or new-parent (parent mother)))
	 (daughter (make-bender name
				(property mother :instruments)
				:section parent
				:no-thin (property mother :no-thin)
				:remarks (remarks mother))))
    (copy-part-properties mother daughter)
    (setf (bender-states daughter)(clone (bender-states mother)))
    (copy-time-signature mother daughter)
    daughter))


(setf (documentation 'make-controllers 'function) +controllers-docstring+)
(setf (documentation 'controllers 'function)
      "MAKE-CONTROLLERS and CONTROLLERS are identical except the later
binds the part to the symbol name while the former does not.  The name 
argument should be a quoted symbol for MAKE-CONTROLLER and unquoted for 
CONTROLLERS.")

(setf (documentation 'make-bender 'function) +bender-docstring+)
(setf (documentation 'bender 'function)
      "MAKE-BENDER and CONTROLLERS are identical except the later
binds the part to the symbol name while the former does not.  The name 
argument should be a quoted symbol for MAKE-CONTROLLER and unquoted for 
BENDER.")

