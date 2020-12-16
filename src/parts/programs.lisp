;;;; CYCO parts programs.lisp
;;;;
;;;; Programs is a type of Part for generating MIDI program change events.
;;;;

(in-package :cyco-part)

(constant +programs-properties+ +part-properties+)

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
:cuefun  - Time cue function, defaults to section value.
:shuffle - Time shuffle function, defaults to section value.
:bars    - Bars per phrase, defaults to section value.
:shift   - Metric-expression, time-shift added to each event, default 0.
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
       
    (defun make-programs (name instruments &key time cuefn shuffle section bars remarks render-once shift)
      docstring
      (let* ((parent (validate-parent-section section))
	     (new-programs-part (make-instance 'programs
				 :properties +programs-properties+
				 :name name
				 :remarks (->string (or remarks "")))))   
	(connect parent new-programs-part)
	(put new-programs-part :cue-function (or cuefn #'bar))
	(put new-programs-part :shuffle-function (or shuffle #'no-shuffle))
	(put new-programs-part :bars bars)
	(init-time-signature new-programs-part)
	(let ((event-time (+ (funcall (property new-programs-part :cue-function) new-programs-part time)
			     (funcall (property new-programs-part :shuffle-function) time)))
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
	      (put new-programs-part :shift (scale-time-parameter (or shift 0) new-programs-part))
	      (put new-programs-part :reversible nil)
	      (setf prognum (or prognum :default))
	      (setf bank (or bank :default))
	      (setf midi-events (append midi-events 
					(program-change-events instrument event-time 
							       :bank bank :program prognum))) ))
	  (setf (programs-events new-programs-part) (sort-midi-events midi-events))
	  new-programs-part)))))
	  

(defmacro programs (name instruments &key time cuefn shuffle section bars remarks render-once shift)
  "Same as make-programs but binds the programs object to name symbol."
  `(progn
     (part-banner (name ,section) ',name)
     (let ((new-programs-part (make-programs ',name ,instruments
			       :time ,time
			       :section ,section
			       :cuefn ,cuefn
			       :shuffle ,shuffle
			       :bars ,bars
			       :render-once ,render-once
			       :shift ,shift
			       :remarks ,remarks)))
       (defparameter ,name new-programs-part)
       new-programs-part)))

(defmethod clone ((mother programs) &key new-name new-parent)
  (let* ((name (->symbol (string-upcase (sformat (or new-name "~A") (name mother)))))
	 (parent (or new-parent (parent mother)))
	 (daughter (make-programs name (property mother :instruments)
				  :section parent
				  :cuefn (property mother :cue-function)
				  :shuffle (property mother :shuffle-function)
				  :bars (property mother :bars)
				  :render-once (property mother :render-once)
				  :remarks (remarks mother))))
    (copy-part-properties mother daughter)
    (copy-time-signature mother daughter)
    (setf (programs-events daughter)
	  (clone (programs-events mother)))
    daughter))

(defmethod render-once ((programs-part programs) &key (offset 0))
  (if (muted-p programs-part)(return-from render-once '()))
  (let ((midi-events '())
	(time-shift (+ offset (property programs-part :shift))))
    (dolist (event (programs-events programs-part))
      (let ((event-time (car event))
	    (message (cdr event)))
	(push (cons (+ event-time time-shift) message) midi-events)))
    (sort-midi-events midi-events)))


(defmethod connect ((parent programs)(child cyco-node))
  (cyco-type-error
   'connect '?
   child
   (sformat "Attempt to connect NODE ~A to leaf node ~A"
	    (name child)(name parent)))) 


