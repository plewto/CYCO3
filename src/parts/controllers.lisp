;;;; CYCO parts controllers.lisp
;;;;
;;;; CONTROLLERS is a part class for generating MIDI control change events.
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


(labels ((real-event-p 
	  (state)
	  (or (controllers-state-curve state)
	      (controllers-state-single-event state)))
	 
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
				   (t (funcall cuefn part start-spec)
				      (funcall shuffle start-spec))))
		 
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
	    (setf (controllers-state-time-interval state) time-interval)))
	 
	 (process-values 
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

	 ;; Converts controller-number specification to integer.
	 ;; The value 'pressure is a special case which returns itself
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

	 (process-cycle-count 
	  (state clause)
	  (setf (controllers-state-cycles state)
		(abs (round (or (second clause) 1)))))

	 (process-phase
	  (state clause)
	  (setf (controllers-state-phase state)
			      (rem (round (or (second clause) 0)) 360)))

	 (process-pulse-width 
	  (state clause)
	  (setf (controllers-state-width state)
		(limit (or (second clause) 0.5) 0.0 1.0)))

	 ;; :cc time controller-number value
	 (add-single-event
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

	 (add-curve-event (curve-type state)
			  (setf (controllers-state-curve state) curve-type))
	 	 
	 (dispatch-event
	  (part state clause)
	  (let ((command (car clause)))
	    (cond
	     ((eq command :reset)(reset state))
	     ((eq command :time) (process-times part state clause))
	     ((eq command :value) (process-values part state clause))
	     ((eq command :ctrl) (process-controller-type state clause))
	     ((eq command :cycles) (process-cycle-count state clause))
	     ((eq command :phase) (process-phase state clause))
	     ((eq command :width) (process-pulse-width state clause))
	     ((eq command :cc) (add-single-event part state clause))
	     ((eq command :ramp) (add-curve-event :ramp state))
	     ((eq command :saw) (add-curve-event :saw state))
	     ((eq command :tri) (add-curve-event :tri state))
	     ((eq command :pulse) (add-curve-event :pulse state))
	     (t (cyco-composition-error
		 'make-controllers
		 (sformat "Invalid controllers clause ~A" clause))))))
	 
	 (process-events (part event-list)
			 (let* ((acc '())
				(state (make-controllers-state)))
			   (reset state)
			   (dolist (event event-list)
			     (soft-reset state)
			     (setf (controllers-state-source state) event)
			     (dolist (clause (partition-list event))
			       (dispatch-event part state clause)
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
	    (process-events part (->list events)))
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

(defmethod clone ((mother controllers) &key new-name new-parent)
  (let* ((frmt (or new-name "~A"))
	 (name (->symbol (sformat frmt (name mother))))
	 (parent (or new-parent (parent mother)))
	 (daughter (make-controllers name
				     (property mother :instruments)
				     :section parent
				     :no-thin (property mother :no-thin)
				     :remarks (remarks mother)
				     :events (clone (property mother :events)))))
    (copy-part-properties mother daughter)
    (copy-time-signature mother daughter)
    daughter))
    
(setf (documentation 'make-controllers 'function) +controllers-docstring+)
(setf (documentation 'controllers 'function)
      "CONTROLLERS and MAKE-CONTROLLERS are identical except the former
binds the part to the symbol name while the later does not.  The name 
argument should be quoted symbol for MAKE-CONTROLLER and unquoted for CONTROLLERS.")
