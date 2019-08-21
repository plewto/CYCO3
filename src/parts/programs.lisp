;;;; CYCO
;;;; Programs is a type of Part for generating MIDI program change events.
;;;;
;;;; Programs are always leaf nodes

(in-package :cyco-part)

(constant +programs-properties+
	  (append +part-properties+
		  '(:render-once
		    :shift)))

(defclass programs (part)
  ((events
    :type list
    :accessor programs-events
    :initform '())))

(let ((docstring 
       "Creates new PROGRMS instance.
name - Symbol
instruments - List of instruments
   For default programs add only the instrument to the list.
     (list bass organ piano ...)

   For non-default programs a nested list is used.
     (list bass (list organ 34) piano)

   In the first example the default organ program is used.
   In the second case the program-number 34 is used.

   For program with bank change use the form (instrument bank program)
      (list bass (list organ bank-number program-number) piano ...)

:time    - Event time in format required by cuing function.
           The cuing function is inherited from the parent Section.
:section - Parent section, defaults to current-section of *project*
:remarks - text
:render-once - If true program events are only rendered once by render-n

Programs are always a leaf node."))
  (flet ((validate-parent-section (parent)
				  (cond ((section-p parent)
					 parent)
					((project-p *project*)
					 (property *project* :current-section))
					(t (cyco-composition-error 'make-programs
								   "No default project")))))		  
       
    (defun make-programs (name instruments &key time section remarks render-once)
      docstring
      (let* ((parent (validate-parent-section section))
	     (new-programs-part (make-instance 'programs
				 :properties +programs-properties+
				 :name name
				 :remarks (->string (or remarks "")))))   
	(connect parent new-programs-part)
	(let ((event-time (funcall (property new-programs-part :cue-function) new-programs-part time))
	      (midi-events '()))
	  ;; ISSUE: A confused mess~
	  (dolist (instrument (->list instruments))
	    (let* ((program-specification-list (->list instrument))
		   (count (length program-specification-list))
		   (instrument (first program-specification-list))
		   (prognum nil)
		   (bank nil))
	      (cond ((= count 1)		; (instrument)
		     nil)			;     use implicit program-number
		    ((= count 2)		; (instrument program)
		                                ;     use explicit program-number
		     (setf prognum (second program-specification-list)))
		    ((>= count 3)		; (instrument bank program)
		                                ;     add bank change
		     (setf prognum (second program-specification-list))
		     (setf bank (third program-specification-list))) )
	      (put new-programs-part :render-once render-once)
	      (put new-programs-part :shift 0.0) ;; constant
	      (put new-programs-part :reversible nil)
	      (setf prognum (or prognum :default))
	      (setf bank (or bank :default))
	      (setf midi-events (append midi-events 
					(program-change-events instrument event-time 
							       :bank bank :program prognum))) ))
	  (setf (programs-events new-programs-part) (sort-midi-events midi-events))
	  new-programs-part)))))
	  

(defmacro programs (name instruments &key time section remarks render-once)
  "Same as make-programs but binds the programs object to name symbol."
  `(progn
     (part-banner (name ,section) ',name)
     (let ((new-programs-part (make-programs ',name ,instruments
			       :time ,time
			       :section ,section
			       :render-once ,render-once
			       :remarks ,remarks)))
       (defparameter ,name new-programs-part)
       new-programs-part)))

(defmethod clone ((source programs) &key new-name new-parent)
  (let* ((name (->symbol (string-upcase (sformat (or new-name "~A") (name source)))))
	 (parent (or new-parent (parent source)))
	 (new-programs-part (make-programs name (property source :instruments)
			     :section parent
			     :render-once (property source :render-once)
			     :remarks (remarks source))))
    (copy-time-signature source new-programs-part)
    (setf (programs-events new-programs-part)
	  (clone (programs-events source)))
    new-programs-part))


(defmethod render-once ((programs-part programs) &key (offset 0))
  (if (not (muted-p programs-part))
      (let ((midi-events '()))
	(dolist (event (programs-events programs-part))
	  (let ((event-time (car event))
		(message (cdr event)))
	    (push (cons (+ event-time offset) message) midi-events)))
	(sort-midi-events midi-events))))

(defmethod render-n ((programs-part programs)(n integer) &key (offset 0.0))
  (let ((period (duration programs-part))
	(template (render-once programs-part))
	(midi-events '()))
    (dotimes (i (if (property programs-part :render-once) 1 n))
      (let ((time-shift (+ offset (* i period))))
	(dolist (event template)
	  (let ((relative-time (car event))
		(message (cdr event)))
	    (push (cons (+ time-shift relative-time) message) midi-events)))))
    (sort-midi-events midi-events)))

(defmethod connect ((parent programs)(child cyco-node))
  (cyco-type-error
   'connect '?
   child
   (sformat "Attempt to connect NODE ~A to leaf node ~A"
	    (name child)(name parent)))) 


