;;;; CYCO parts sysex.lisp
;;;;
;;;; Defines SYSEX part.
;;;;

(in-package :cyco-part)

(constant +sysex-part-properties+ +part-properties+)

(defclass sysex (part)
  ((events
    :type list
    :initform '()
    :accessor sysex-events)))
   
(defgeneric sysex-p (object))
(defmethod sysex-p ((object t)) nil )
(defmethod sysex-p ((object sysex)) t)


(labels ((process-time (part clause)
		       (let* ((cue (property part :cue-function))
			      (shuffle (property part :shuffle-function))
			      (time-spec (first clause)))
			 (+ (funcall cue part time-spec)
			    (funcall shuffle time-spec))))


	 ;; ((x x x) data....)
	 (process-events (part events)
			 (let ((acc '()))
			   (dolist (event events)
			     (let ((time (process-time part event))
				   (data (->vector (append (cdr event) '(#xF7)))))
			       (push (cons time (midi-system-exclusive data)) acc)))
			   acc)) )
				   
					 

	(defun make-sysex-part (name &key section cuefn shuffle shift render-once remarks events)
	  (let* ((part-name (->cyco-symbol name))
		 (parent (validate-section part-name section))
		 (sysex-part (make-instance  'sysex
					     :properties +sysex-part-properties+
					     :name part-name
					     :remarks (->string (or remarks ""))
					     :transient t)))
	    (connect parent sysex-part)
	    (init-time-signature sysex-part)
	    (put sysex-part :cue-function cuefn)
	    (put sysex-part :shuffle-function shuffle)
	    (put sysex-part :render-once render-once)
	    (put sysex-part :transposable nil)
	    (put sysex-part :reversible nil)
	    (put sysex-part :muted nil)
	    (put sysex-part :shift (scale-time-parameter (or shift 0) sysex-part))
	    (setf (sysex-events sysex-part)
		  (process-events sysex-part events))
	    sysex-part))) 

(defmacro sysex (name &key section cuefn shuffle shift render-once remarks events)
  `(progn
     (part-banner (name ,section) ',name)
     (let ((new-part (make-sysex-part ',name
				      :section ,section
				      :cuefn ,cuefn 
				      :shuffle ,shuffle 
				      :shift ,shift 
				      :render-once ,render-once 
				      :remarks ,remarks 
				      :events ,events)))
       (defparameter ,name new-part)
       new-part)))

(defmethod clone ((mother sysex) &key new-name new-parent)
  (let* ((frmt (or new-name "~A"))
	 (name (->symbol (sformat frmt (name mother))))
	 (parent (or new-parent (parent mother)))
	 (daughter (make-sysex-part name :section parent
				    :cuefn (property mother :cue-function)
				    :shuffle (property mother :shuffle-function)
				    :shift (property mother :shift)
				    :render-once (property mother :render-once)
				    :remarks (remarks mother)
				    :events nil)))
    (copy-part-properties mother daughter)
    (copy-time-signature mother daughter)
    (setf (sysex-events daughter)
	  (clone (sysex-events mother)))
    (reset daughter)
    daughter))
    
(defmethod render-once ((part sysex) &key (offset 0.0) &allow-other-keys)
  (if (muted-p part)(return-from render-once '()))
  (let* ((midi-events '())
	 (time-shift (+ offset (property part :shift))))
    (dolist (event (sysex-events part))
      (push (cons (+ time-shift (car event)) (cdr event)) midi-events))
    (sort-midi-events midi-events)))
      
	    
(setf (documentation 'make-sysex 'function)
      +make-sysex-docstring+)

(setf (documentation 'sysex 'function)
      +sysex-docstring+)
