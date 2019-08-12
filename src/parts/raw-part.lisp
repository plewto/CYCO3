;;;; CYCO
;;;;


(constant +raw-part-properties+
	  (append +part-properties+
		  '(:shift
		    :render-once)))

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


(let ((docstring
       "Creates new instance of RAW-PART named name.
:events - event list, see below for format.
:bars - number of bars per phrase, defaults to parent section value.
:beats - number of beats per bar, defaults to parent section value.
:render-once - bool, if true the part events are rendered only once
when the section is rendered.  For example if the section has length of 4 bars
and the part has a length of 2, the part is repeated twice unless render-once
is true.   Default nil.
:transposable - bool, if nil the part is immune to transpose and invert 
operations.
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
	(setf (event-list new-raw-part) (->list events))
	(put new-raw-part :transposable transposable)
	(put new-raw-part :bars bars)
	(put new-raw-part :beats beats)
	(put new-raw-part :render-once render-once)
	(put new-raw-part :shift 0.0) ;; constant 0
	(put new-raw-part :reversible nil)
	(connect parent-section new-raw-part)
	(set-cyco-prompt)
	(if (validate-event-list new-raw-part events)
	    (setf (event-list new-raw-part) events))
	new-raw-part))))
	
(defmacro raw-part (name &key
			 events
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
					:events ,events
					:bars ,bars
					:beats ,beats
					:render-once ,render-once
					:transposable ,transposable
					:section ,section
					:remarks (->string (or ,remarks "")))))
       (defparameter ,name new-raw-part)
       new-raw-part)))

(defmethod clone ((source raw-part) &key new-name new-parent)
  (let* ((name (->symbol (string-upcase (sformat (or new-name "CLONE-OF-~A")
						 (name source)))))
	 (parent (or new-parent (parent source)))
	 (new-raw-part (make-raw-part name
			     :events '()
			     :bars (bars source)
			     :beats (beats source)
			     :transposable (property source :transposable)
			     :section parent
			     :remarks (remarks source))))
    (copy-time-signature source new-raw-part)
    (setf (event-list new-raw-part) (clone (event-list source)))
    new-raw-part))

(defmethod render-once ((raw-part raw-part) &key (offset 0.0))
  (let ((midi-events '()))
    (if (not (muted-p raw-part))
	(progn 
	  (dolist (event (event-list raw-part))
	    (push (cons (+ offset (car event)) (cdr event)) midi-events))
	  (sort-midi-events midi-events)))))

(defmethod render-n ((raw-part raw-part)(n integer) &key (offset 0.0))
  (let ((period (phrase-duration raw-part))
	(template (render-once raw-part))
	(midi-events '()))
    (dotimes (i (if (property raw-part :render-once) 1 n))
      (let ((time-shift (+ (* i period) offset)))
	(dolist (event template)
	  (let ((relative-time (car event))
		(message (cdr event)))
	    (push (cons (+ time-shift relative-time) message) midi-events)))))
    (sort-midi-events midi-events)))

(defmethod connect ((parent raw-part)(child cyco-node))
  (cyco-type-error
   'connect '?
   child
   (sformat "Attempt to connect NODE ~A to leaf node ~A"
	    (name child)(name parent))))

