;;;; CYCO parts text.lisp
;;;;
;;;; Defines TEXT-PART for inserting meta text messages in Sections.
;;;;

(in-package :cyco-part)

(constant +text-part-properties+ +part-properties+)

(defclass text-part (part)
  ((text-events
    :type list
    :accessor text-events
    :initform '())))

(defgeneric text-part-p (object))
(defmethod text-part-p ((object t)) nil )
(defmethod text-part-p ((object text-part)) t)


(let ((text-types '(:text :copyright
			  :track-name :instrument-name
			  :lyric :marker :cue)))
  
  (labels ((validate-text-type
	    (text-type)
	    (if (member text-type text-types)
		text-type
	      (progn
		(cyco-warning
		 (format nil "Invalid meta text-type  ~A  using default :TEXT" text-type))
		:text)))
	   
	   ;; :time time-specification
	   (process-time
	    (text-part clause)
	    (let* ((cue-function (property text-part :cue-function))
		   (time-spec (first clause)))
	      (funcall cue-function text-part time-spec)))

	   (process-events (part events)
			   (let ((acc '()))
			     (dolist (event events)
			       (let ((time (process-time part event))
				     (ttype (validate-text-type (second event)))
				     (text (format nil "~A" (third event))))
				 (push (list time ttype text) acc)))
			     (reverse acc))) )
	  
	  (defun make-text-part (name &key section cuefn shift render-once remarks events)
	    (let* ((part-name (->cyco-symbol name))
		   (parent (validate-section part-name section))
		   (new-text-part (make-instance 'text-part
						 :properties +text-part-properties+
						 :name part-name
						 :remarks (->string (or remarks ""))
						 :transient t)))
	      (connect parent new-text-part)
	      (init-time-signature new-text-part)
	      (put new-text-part :cue-function cuefn)
	      (put new-text-part :render-once render-once)
	      (put new-text-part :transposable nil)
	      (put new-text-part :reversible nil)
	      (put new-text-part :muted nil)
	      (put new-text-part :shift (scale-time-parameter (or shift 0) new-text-part))
	      (setf (text-events new-text-part)
		    (process-events new-text-part (->list events)))
	      new-text-part)) ))
	    
(defmacro text-part (name &key section cuefn shift render-once remarks events)
  `(progn
     (part-banner (name ,section) ',name)
     (let ((new-text-part (make-text-part ',name :section ,section
					  :cuefn ,cuefn :shift ,shift
					  :render-once ,render-once
					  :remarks ,remarks
					  :events ,events)))
       (defparameter ,name new-text-part)
       new-text-part)))


(defmethod render-once ((part text-part) &key (offset 0.0) &allow-other-keys)
  (if (muted-p part)(return-from render-once '()))
  (let ((midi-events '())
	(time-shift (+ offset (property part :shift))))
    (dolist (event (text-events part))
      (let* ((time (car event))
	     (ttype (second event))
	     (text (third event))
	     (message (cond ((eq ttype :copyright)(midi-meta-copyright text))
			    ((eq ttype :track-name)(midi-meta-track-name text))
			    ((eq ttype :instrument-name)(midi-meta-instrument-name text))
			    ((eq ttype :lyric)(midi-meta-lyric text))
			    ((eq ttype :marker)(midi-meta-marker text))
			    ((eq ttype :cue)(midi-meta-cue text))
			    (t (midi-meta-text text)))))
	(push (cons (+ time time-shift) message) midi-events)))
    (sort-midi-events midi-events)))


(defmethod clone ((mother text-part) &key new-name new-parent)
  (let* ((frmt (or new-name "~A"))
	 (name (->symbol (sformat frmt (name mother))))
	 (parent (or new-parent (parent mother)))
	 (daughter (make-text-part name :section parent
				   :cuefn (property mother :cue-function)
				   :shift (property mother :shift)
				   :render-once (property mother :render-once)
				   :remarks (remarks mother)
				   :events nil)))
    (copy-part-properties mother daughter)
    (copy-time-signature mother daughter)
    (setf (text-events daughter)
	  (clone (text-events mother)))
    (reset daughter)
    daughter))
    
