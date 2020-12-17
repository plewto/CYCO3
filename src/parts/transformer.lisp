;;;; CYCO part transformer.lisp
;;;;
;;;; MIDI data transformer.
;;;;
;;;; TRANSFORMER is a part which iterates over a MIDI event list or the
;;;; output from another part, and modifies selected events.
;;;;

(in-package :cyco-part)

(constant +transformer-properties+
	   +part-properties+ )

(defclass transformer (part)
  ((event-template
    :type list
    :initform '()
    :accessor transformer-template
    :initarg :template)
   ;; The filter function discriminates which events to process.
   ;; It should return true for processed events.
   ;;
   (filter
    :type function
    :initform #'(lambda (part event) (dismiss part) event)
    :accessor transformer-filter-function
    :initarg :filter)
   ;; The transform function takes the events indicated by the filter
   ;; and returns a modified version.   It may return
   ;;    * nil
   ;;    * event cons - (time . midi-message)
   ;;    * list of event cons
   ;;
   (transform-function
    :type function
    :initform #'(lambda (part event) (dismiss part) event)
    :accessor transformer-transform-function
    :initarg :transform)))
 

(defgeneric transformer-p (item))
(defmethod transformer-p ((item t)) nil)
(defmethod transformer-p ((transformer transformer)) t)


   
(defun make-transformer (name source &key
			      filter
			      transform
			      render-once
			      remarks)
  (let* ((parent (cond ((part-p source)
			(parent source))
		       (t (property *project* :current-section))))
	 (template (cond ((part-p source)
			  (let ((p (clone source)))
			    (mute p :unmute)
			    (prog1
				(render-once p)
			      (disconnect p))))
			 ((listp source)
			  (clone source))
			 (t (cyco-type-error
			     'make-transformer
			     (sformat "Illegal source argument to transformer ~A" name)
			     (sformat "~A" source))
			    '())))
	 (part (make-instance 'transformer
			      :properties +transformer-properties+
			      :name (->cyco-symbol name)
			      :remarks (->string (or remarks ""))
			      :transient t
			      :template template
			      :filter (or filter
					  #'(lambda (part event)(dismiss part event) (list event)))
			      :transform (or transform
					     #'(lambda (part event)(dismiss part)(list event))))))
			      
    (connect parent part)
    (init-time-signature part)
    (put part :render-once render-once)
    (put part :transposable nil)
    (put part :reversible nil)
    part)) 
    
			       
(defmacro transformer (name source &key
			     filter
			     transform
			     render-once
			     remarks)
  `(progn 
     (part-banner "" ',name)
     (let ((part (make-transformer ',name ,source
				   :filter ,filter
				   :transform ,transform
				   :render-once ,render-once
				   :remarks ,remarks)))
       (defparameter ,name part)
       part)))



(defmethod clone ((mother transformer) &key new-name new-parent)
  (let* ((frmt (or new-name "~A"))
	 (name (->cyco-symbol (sformat frmt (name mother))))
	 (parent (or new-parent (parent mother)))
	 (daughter (make-transformer name (transformer-template mother)
				     :filter (transformer-filter-function mother)
				     :transform (transformer-transform-function mother)
				     :render-once (property mother :render-once)
				     :remarks (remarks mother))))
    (connect parent daughter)
    (init-time-signature daughter)
    daughter))
				    

(defmethod render-once ((part transformer) &key (offset 0.0))
  (let ((midi-events '())
	(filter (transformer-filter-function part))
	(transform (transformer-transform-function part)))
    (dolist (event (transformer-template part))
      (if (funcall filter part event)
	  (dolist (xevent (->list (funcall transform part event)))
	    (push (cons (+ offset (car xevent))(cdr xevent)) midi-events))))
    (sort-midi-events midi-events)))


(setf (documentation 'make-transformer 'function) +transformer-docstring+)
(setf (documentation 'transformer 'function)
      "MAKE-TRANSFORMER and TRANSFORMER are identical except the later
binds the new part to the symbol name, while the former does not.  The
name argument is a quoted symbol for MAKE-TRANSFORMER and unquoted for TRANSFORMER.")
