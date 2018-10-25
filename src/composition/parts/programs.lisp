;;;; PigIron CYCO composition parts programs
;;;; Programs is a type of Part for generating MIDI program change events.
;;;;
;;;; Programs are always leaf nodes

(constant +programs-properties+
	  (append +part-properties+
		  '(:render-once
		    :shift)))

;; If render-once property is true, the program events are only
;; renderd one-time by the render-n method.

(defclass programs (part)
  ((events
    :type list ; of MIDI events
    :accessor programs-events
    :initform '())))

(defun make-programs (name instruments &key time section remarks render-once)
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

Programs are always a leaf node."

  (let* ((parent (cond ((section-p section)
			section)
		       ((project-p *project*)
			(property *project* :current-section))
		       (t (cyco-no-project-error 'make-programs))))
	 (obj (make-instance 'programs
			     :properties +programs-properties+
			     :name name
			     :remarks (->string (or remarks "")))))   
    (connect parent obj)
    (let ((event-time (funcall (property obj :cue-function) obj time))
	  (acc '()))
      (dolist (inst (->list instruments))
	(let* ((plist (->list inst))
	       (count (length plist))
	       (inst (first plist))
	       (prognum nil)
	       (bank nil))
	  (cond ((= count 1)		; (instrument)
		 nil)
		((= count 2)		; (instrument program)
		 (setf prognum (second plist)))
		((>= count 3)		; (instrument bank program)
		 (setf prognum (second plist))
		 (setf bank (third plist))) )
	  (put obj :render-once render-once)
	  (put obj :shift 0.0) ;; constant
	  (put obj :reversible nil)
	  (setf prognum (or prognum :default))
	  (setf bank (or bank :default))
	  (setf acc (append acc (program-change-events inst event-time :bank bank :program prognum)))
	  ))
      (setf (programs-events obj) (sort-midi-events acc))
      obj)))
	  

(defmacro programs (name instruments &key time section remarks render-once)
  "Same as make-programs but binds the programs object to name symbol."
  `(progn
     (part-banner (name ,section) ',name)
     (let ((obj (make-programs ',name ,instruments
			       :time ,time
			       :section ,section
			       :render-once ,render-once
			       :remarks ,remarks)))
       (defparameter ,name obj)
       obj)))

(defmethod clone ((src programs) &key new-name new-parent)
  (let* ((frmt (or new-name "~A"))
	 (name (->symbol (string-upcase (sformat frmt (name src)))))
	 (parent (or new-parent (parent src)))
	 (prt (make-programs name (property src :instruments)
			     :section parent
			     :render-once (property src :render-once)
			     :remarks (remarks src))))
    (copy-time-signature src prt)
    (setf (programs-events prt)
	  (clone (programs-events src)))
    prt))

(defmethod render-once ((obj programs) &key (offset 0))
  (if (not (muted-p obj))
      (let ((acc '()))
	(dolist (evn (programs-events obj))
	  (let ((st (car evn))
		(msg (cdr evn)))
	    (push (cons (+ st offset) msg) acc)))
	(sort-midi-events acc))))

(defmethod render-n ((obj programs)(n integer) &key (offset 0.0))
  (let ((period (duration obj))
	(template (render-once obj))
	(acc '()))
    (dotimes (i (if (property obj :render-once) 1 n))
      (let ((tshift (+ offset (* i period))))
	(dolist (p template)
	  (let ((reltime (car p))
		(msg (cdr p)))
	    (push (cons (+ tshift reltime) msg) acc)))))
    (sort-midi-events acc)))
	
    

(defmethod connect ((parent programs)(child cyco-node))
  (cyco-not-implemented-error
   'connect parent
   "Attempt to add child to leaf Programs node."))
   
