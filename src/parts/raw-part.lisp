;;;; CYCO parts raw-part.lisp
;;;;
;;;; Defines primitive RAW-PART class for explicit MIDI event specification.
;;;;

(in-package :cyco-part)

(constant +raw-part-properties+ +part-properties+)

(defclass raw-part (part)
  ((event-list
    :type list
    :accessor event-list
    :initform '()
    :initarg :events))
  (:documentation
   "A RAW-PART is a primitive type of PART where each MIDI event is 
explicitly specified.  Typically you'll want to use some other part 
type.  Raw-parts are intended as a fallback where some odd combination
of MIDI events not addressed by the other part types is required.

Raw-parts can not have sub-parts."))


(defgeneric raw-part-p (object))
(defmethod raw-part-p ((object t)) nil)
(defmethod raw-part-p ((object raw-part)) t)

(let ((docstring
       "Creates new instance of RAW-PART named name.
:events - event list, see below for format.
:bars - number of bars per phrase, defaults to parent section value.
:beats - number of beats per bar, defaults to parent section value.
:render-once - bool, if true the part events are rendered only once
      when the section is rendered.  For example if the section has length of 4 bars
      and the part has a length of 2, the part is repeated twice unless render-once
      is true.   Default nil.
:transposable - bool, if nil the part is immune to transpose and invert operations.
:shift   - metric-expression, this shift added to each event.
:section - parent section, defaults to current section of *project*
:remarks - optional remarks text.

Raw-part events are specified as a nested list of MIDI events.
  
   ((time-1 . midi-message-1)
    (time-2 . midi-message-2)
     .......................
    (time-n . midi-message-n))

The time values are offsets in seconds from the start of the part and are not
subject to the Section cueing function."))   

  (labels ((validate-event (event)
			   (and (consp event)
				(numberp (car event))
				(midi-message-p (cdr event))))
	   
	   (validate-event-list (part events)
				(dolist (event events)
				  (if (not (validate-event event))
				      (progn 
					(cyco-composition-error
					 'make-raw-part
					 (sformat "RAW-PART ~A" (name part))
					 (sformat "Malformed part event: ~A" event))
					(return-from validate-event-list nil))))
				t)

	   (validate-section (section)
			     (or (and (section-p section) section)
				 (and (project-p *project*)
				      (or (property *project* :current-section))))))
    
    (defun make-raw-part (name &key
			       events
			       shift
			       bars
			       beats
			       render-once
			       (transposable t)
			       section
			       (remarks ""))
      docstring
      (let ((parent-section (or (validate-section section)
				(progn 
				  (cyco-composition-error 'make-raw-part "No current project")
				  (return-from make-raw-part))))
	    (new-raw-part (make-instance 'raw-part
				 :name name
				 :properties +raw-part-properties+
				 :remarks (->string remarks))))
	(connect parent-section new-raw-part)
	(setf (event-list new-raw-part) (->list events))
	(put new-raw-part :transposable transposable)
	(put new-raw-part :bars bars)
	(put new-raw-part :beats beats)
	(init-time-signature new-raw-part)
	(put new-raw-part :render-once render-once)
	(put new-raw-part :shift (scale-time-parameter (or shift 0) new-raw-part))
	(put new-raw-part :reversible nil)
	(put new-raw-part :cuelist (loop for e in events collect (car e)))
	(if (validate-event-list new-raw-part events)
	    (setf (event-list new-raw-part) events))
	new-raw-part))))
	
(defmacro raw-part (name &key
			 events
			 shift
			 bars
			 beats
			 render-once
			 (transposable t)
			 section
			 remarks)
  "Same as make-raw-part except binds the part to a symbol named name."
  `(progn
     (part-banner (name ,section) ',name)
     (let ((new-raw-part (make-raw-part ',name
					:shift ,shift
					:events ,events
					:bars ,bars
					:beats ,beats
					:render-once ,render-once
					:transposable ,transposable
					:section ,section
					:remarks (->string (or ,remarks "")))))
       (defparameter ,name new-raw-part)
       new-raw-part)))

(defmethod clone ((mother raw-part) &key new-name new-parent)
  (let* ((frmt (or new-name "~A"))
	 (name (->symbol (sformat frmt (name mother))))
	 (parent (or new-parent (parent mother)))
	 (daughter (make-raw-part name
				  :events '()
				  :shift (property mother :shfit)
				  :bars (bars mother)
				  :beats (beats mother)
				  :transposable (property mother :transposable)
				  :section parent
				  :remarks (remarks mother))))
    (copy-part-properties mother daughter)
    (copy-time-signature mother daughter)
    (setf (event-list daughter) (clone (event-list mother)))
    (reset daughter)
    daughter))


(defmethod render-once ((raw-part raw-part) &key (offset 0.0) &allow-other-keys)
  (if (muted-p raw-part)(return-from render-once '()))
  (let ((midi-events '())
	(time-shift (+ offset (property raw-part :shift))))
    (dolist (event (event-list raw-part))
      (push (cons (+ time-shift (car event)) (cdr event)) midi-events))
    (sort-midi-events midi-events)))

(defmethod connect ((parent raw-part)(child cyco-node))
  (cyco-type-error
   'connect '?
   child
   (sformat "Attempt to connect NODE ~A to leaf node ~A"
	    (name child)(name parent))))


