;;;; CYCO parts simple-part.lisp
;;;;
;;;; SIMPLE-PART generates MIDI events from a straight forward event list.
;;;; See STRUMMER for more advanced features.
;;;;

(in-package :cyco-part)

(constant +simple-part-properties+
	  (append +part-properties+
		  '(:shift
		    :shuffle-function
		    :render-once)))

(defclass simple-part (part)
  ((states
    :type list
    :accessor simple-part-states
    :initform '())))

(defgeneric simple-part-p (item))
(defmethod simple-part-p ((item t)) nil)
(defmethod simple-part-p ((item simple-part)) t)

(let ((argument-count-table (make-hash-table :size 12)))
  (setf (gethash :reset argument-count-table) 0)
  (setf (gethash :time argument-count-table) 1)
  (setf (gethash :chord argument-count-table) 1)
  (setf (gethash :inversion argument-count-table) 1)
  (setf (gethash :octave argument-count-table) 1)
  (setf (gethash :dur argument-count-table) 1)
  (setf (gethash :amp argument-count-table) 1)
  (setf (gethash :key argument-count-table) 1)
  (setf (gethash :pressure argument-count-table) 1)
  (setf (gethash :cc argument-count-table) 2)
  (setf (gethash :bend argument-count-table) 1)
  (setf (gethash :program argument-count-table) 1)
  (setf (gethash :bank argument-count-table) 2)

  (labels ((process-time (part state clause)
			 (let* ((cuefn (property part :cue-function))
				(shuffle (property part :shuffle-function))
				(time-specification (second clause))
				(time (+ (funcall cuefn part time-specification)
					 (funcall shuffle time-specification))))
			   (setf (simple-state-time-specification state) time-specification
				 (simple-state-time state) time)))
	   
	   (process-chord-by-name (part state event clause chord-name)
				  (let ((chord-model (property part :chord-model)))
				    (if (defines-chord-p chord-model (->cyco-symbol chord-name))
					(setf (simple-state-chord-type state) chord-name)
				      (cyco-composition-error
				       'make-simple-part
				       (part-id part)
				       (clause-id event clause)
				       (sformat "Chord model ~A does not define chord ~A"
						(name chord-model) chord-name)))))
	   
	   (process-chord-as-template (state template)
				      (setf (simple-state-chord-type state) template))


	   (process-chord (part state event clause)
			  (let ((chord-specification (second clause)))
			    (if (listp chord-specification)
				(process-chord-as-template state chord-specification)
			      (process-chord-by-name part state event clause chord-specification))))

	   (dispatch-event
	    (part state event clause)
	    (let ((command (validate-argument-count part event clause argument-count-table)))
	      (cond 
	       ((eq command :reset)(reset state))
	       ((eq command :time)
		(process-time part state clause))
	       ((eq command :key)
		(setf (simple-state-key state)(second  clause)))
	       ((eq command :chord)
		(process-chord part state event clause))
	       ((eq command :inversion)
		(setf (simple-state-chord-inversion state)
		      (expect-integer part event clause :min -12 :max +12 :default 0)))
	       ((eq command :octave)
		(setf (simple-state-chord-octave state)
		      (expect-integer part event clause :min -3 :max +3 :default 0)))
	       ((eq command :dur)
		(setf (simple-state-articulation state)
		      (expect-metric-expression part event clause)))
	       ((eq command :amp)
		(setf (simple-state-dynamic state)(second clause)))
	       ((eq command :pressure)
		(setf (simple-state-pressure state)
		      (expect-normalized-float part event clause)))
	       ((eq command :cc)
		(let ((controller-number (expect-integer part event clause :position 1 :min 0 :max 127))
		      (value (expect-normalized-float part event clause :position 2)))
		  (if (and controller-number value)
		      (setf (simple-state-controller-number state) controller-number
			    (simple-state-controller-value state) value))))
	       ((eq command :bend)
		(setf (simple-state-bend state)
		      (expect-normalized-float part event clause :signed t))) 
	       ((eq command :program)
		(setf (simple-state-program-number state)
		      (second clause))) 
	       ((eq command :bank)
		(setf (simple-state-program-bank state)(second clause)
		      (simple-state-program-number state)(third clause))) )))
	
	   (real-event-p (state)
			 (or (simple-state-key state)
			     (simple-state-pressure state)
			     (and (simple-state-controller-number state)
				  (simple-state-controller-value state))
			     (simple-state-bend state)
			     (simple-state-program-number state)))

	   (process-events (simple-part event-list)
			   (let ((acc '())
				 (state (make-simple-state)))
			     (reset state)
			     (dolist (event event-list)
			       (setf (simple-state-source state) event)
			       (dolist (clause (partition-list event))
				 (dispatch-event simple-part state event clause))
			       (if (real-event-p state)
				   (progn 
				     (push (clone state) acc)
				     (soft-reset state))))
			     (reverse acc))) )
  
    (defun make-simple-part (name instruments &key
				  section
				  cuefn
				  shuffle
				  shift
				  tempo unit bars beats subbeats
				  render-once
				  transposable
				  reversible
				  chord-model
				  remarks
				  events)
      (let* ((part-name (->cyco-symbol name))
	     (parent (validate-section part-name section))
	     (new-part (make-instance 'simple-part
				      :properties +simple-part-properties+
				      :name part-name
				      :remarks (->string (or remarks ""))
				      :transient t)))
	(put new-part :instruments (->list instruments))
	(put new-part :cue-function cuefn)
	(put new-part :shuffle-function shuffle)
	(put new-part :shift (or shift 0.0))
	(put new-part :tempo tempo)
	(put new-part :unit unit)
	(put new-part :bars bars)
	(put new-part :beats beats)
	(put new-part :subbeats subbeats)
	(put new-part :render-once render-once)
	(put new-part :transposable transposable)
	(put new-part :reversible reversible)
	(put new-part :chord-model chord-model)
	(connect parent new-part)
	(setf (simple-part-states new-part)
	      (process-events new-part (->list events)))
	new-part)) ))

(setf (documentation 'make-simple-part 'function) +make-simple-part-docstring+)

(defmacro simple-part (name instruments &key
			    section
			    cuefn
			    shuffle
			    shift
			    tempo unit bars beats subbeats
			    render-once
			    transposable
			    reversible
			    chord-model
			    remarks
			    events)
  `(progn
     (part-banner (name ,section) ',name)
     (let ((new-part (make-simple-part ',name ,instruments
				       :section ,section
				       :cuefn ,cuefn
				       :shuffle ,shuffle
				       :shift ,shift
				       :tempo ,tempo
				       :unit ,unit
				       :bars ,bars
				       :beats ,beats
				       :subbeats ,subbeats
				       :render-once ,render-once
				       :transposable ,transposable
				       :reversible ,reversible
				       :chord-model ,chord-model
				       :remarks ,remarks
				       :events ,events)))
       (defparameter ,name new-part)
       new-part)))
				       
(setf (documentation 'simple-part 'function) +simple-part-docstring+)

(defmethod transpose ((part simple-part)(x t))
  (if (property part :transposable)
      (dolist (state (simple-part-states part))
	(transpose state x)))
  part)

(defmethod invert ((part simple-part)(pivot t))
  (if (and pivot (property part :transposable))
      (dolist (state (simple-part-states part))
	(invert state pivot)))
  part)

(defmethod retrograde ((part simple-part))
  (if (property part :reversible)
      (let ((acc '()))
	(dolist (state (simple-part-states part))
	  (let ((kn (simple-state-key state)))
	    (if kn (push kn acc))))
	(dolist (state (simple-part-states part))
	  (let ((kn (simple-state-key state)))
	    (if kn (setf (simple-state-key state)(pop acc)))))))
  part)

(defmethod clone ((mother simple-part) &key new-parent new-name)
  (let* ((name (->cyco-symbol (sformat (or new-name "~A") (name mother))))
	 (parent (or new-parent (parent mother)))
	 (daughter (make-simple-part name (property mother :instruments)
				     :section parent
				     :cuefn (property mother :cue-function)
				     :shuffle (property mother :shuffle-function)
				     :transposable (property mother :transposable)
				     :reversible (property mother :reversible)
				     :chord-model (property mother :chord-model)
				     :render-once (property mother :render-once)
				     :shift (property mother :shift)
				     :remarks (property mother :remarks)
				     :events '())))
    (copy-time-signature mother daughter)
    (setf (simple-part-states daughter)
	  (clone (simple-part-states mother)))
    daughter))
      
	      
