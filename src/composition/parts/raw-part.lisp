;;;; PigIron CYCO composition parts raw-part
;;;;
;;; raw-part is always a leaf node.

(constant +raw-part-properties+
	  (append +part-properties+
		  '(:shift
		    :render-once)))

(defclass raw-part (part)
  ((event-list
    :type list
    :accessor event-list
    :initform '()
    :initarg :events)))
	  
	  
;; Events: nested list  ((time . midi-message)
;;                       (time . midi-message) ...)		
;;

(labels ((validate-event
	  (event)
	  (and (consp event)
	       (numberp (car event))
	       (midi-event-p (cdr event))))
	 
	 (validate-event-list
	  (part events)
	  (dolist (evnt events)
	    (if (not (validate-event evnt))
	 	(progn 
	 	  (cyco-malformed-part-event-error part evnt)
	 	  (return-from validate-event-list nil))))
	  t))
  
  (defun make-raw-part (name &key
			     events
			     bars
			     beats
			     render-once
			     (transposable t)
			     section (remarks ""))
    (let ((sec (or section
  		   (and (project-p *project*)
  			(property *project* :current-section)))))
      (if (not sec)
  	  (cyco-section-does-not-exists-error 'make-raw-part sec name)
  	(let ((part (make-instance 'raw-part
				   :name name
				   :properties +raw-part-properties+
				   :remarks (->string remarks))))
  	  (setf (event-list part) (->list events))
  	  (put part :transposable transposable)
	  (put part :bars bars)
	  (put part :beats beats)
	  (put part :render-once render-once)
	  (put part :shift 0.0) ;; constant 0
	  (put part :reversible nil)
  	  (connect sec part)
  	  (set-cyco-prompt)
  	  (if (validate-event-list part events)
  	      (setf (event-list part) events))
  	  part)))))
	
(defmacro raw-part (name &key
			 events
			 bars
			 beats
			 render-once
			 (transposable t)
			 section
			 remarks)
  `(progn
     (part-banner (name ,section) ',name)
     (let ((prt (make-raw-part ',name
			       :events ,events
			       :bars ,bars
			       :beats ,beats
			       :render-once ,render-once
			       :transposable ,transposable
			       :section ,section
			       :remarks (->string (or ,remarks "")))))
       (defparameter ,name prt)
       prt)))

(defmethod clone ((src raw-part) &key new-name new-parent)
  (let* ((name (->symbol (string-upcase (sformat (or new-name "CLONE-OF-~A")
						 (name src)))))
	 (parent (or new-parent (parent src)))
	 (prt (make-raw-part name
			     :events '()
			     :bars (bars src)
			     :beats (beats src)
			     :transposable (property src :transposable)
			     :section parent
			     :remarks (remarks src))))
    (copy-time-signature src prt)
    (setf (event-list prt) (clone (event-list src)))
    prt))

(defmethod render-once ((part raw-part) &key (offset 0.0))
  (let ((acc '()))
    (if (not (muted-p part))
	(progn 
	  ;; intermediate form  nested list: ((time . midi-message) ...)
	  (dolist (event (event-list part))
	    (push (cons (+ offset (car event)) (cdr event)) acc))
	  (setf acc (sort acc #'(lambda (a b)
				  (let ((time-a (car a))
					(time-b (car b)))
				    (cond ((= time-a time-b)
					   (let ((pa (priority (cdr a)))
						 (pb (priority (cdr b))))
					     (< pa pb)))
					  (t (< time-a time-b)))))))))
    acc))


(defmethod render-n ((part raw-part)(n integer) &key (offset 0.0))
  (let ((period (phrase-duration part))
	(template (render-once part))
	(acc '()))
    (dotimes (i (if (property part :render-once) 1 n))
      (let ((tshift (+ (* i period) offset)))
	(dolist (evn template)
	  (let ((reltime (car evn))
		(msg (cdr evn)))
	    (push (cons (+ tshift reltime) msg) acc)))))
    (sort-midi-events acc)))
    



(defmethod connect ((parent raw-part)(child cyco-node))
  (cyco-not-implemented-error
   'connect parent
   "Attempt to add child to leaf raw-part node."))
