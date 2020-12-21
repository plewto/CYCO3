;;;; CYCO orchestra channel-assignments.lisp
;;;;
;;;; Defines nested symbolic MIDI channel names.
;;;;
;;;;

(in-package :cyco)

(defstruct meta-channel
  name
  value
  remarks)

(let ((assignments '())
      (reverse-assignments (->vector (range 1 17))))

  (defun reset-channel-assignments ()
    "Clear all meta channel assignments."
    (setf assignments '())
    (setf reverse-assignments (->vector (range 1 17)))
    (dotimes (i 16)
      (setf (aref reverse-assignments i)
	    (make-meta-channel
	     :name (1+ i)
	     :value (1+ i) 
	     :remarks ""))))

  (reset-channel-assignments)
  
  (defun meta-channel! (name value &optional (remarks ""))
    "Define new meta MIDI channel.
name  - Symbol 
value - The channel's value may either be an absolute MIDI channel 
        as an integer between 1 and 16 inclusive, or it may refer to 
        a previously defined channel.
remarks - optional explanation."
    (let ((mchan (make-meta-channel
		  :name name
		  :value value
		  :remarks (->string remarks))))
      (push (cons name mchan) assignments)
      (if (and (integerp value)(>= value 1)(<= value 16))
  	  (setf (aref reverse-assignments (1- value)) mchan))
      mchan))

  (defmethod meta-channel ((channel integer) &optional resolve)
    "In range integer channel numbers map to themselves."
    (declare (ignore resolve))
    (if (and (plusp channel)(<= channel 16))
	channel
      (cyco-value-error 'meta-channel channel)))

  (defmethod meta-channel ((channel null) &optional resolve)
    (if resolve 0 nil))

 
  (labels ((resolve-channel (target-name name depth)
			   (if (zerop depth)
			       (cyco-error 'meta-channel! name "Circular channel assignment")
			     (let* ((p (cdr (assoc name assignments)))
				    (chan (and p (meta-channel-value p))))
			       (if (and (integerp chan)(plusp chan)(<= chan 16))
				   chan
				 (resolve-channel target-name chan (1- depth))))))
	   (resolve-channel-once (name)
			     (let ((p (cdr (assoc name assignments))))
			       (or (and p (meta-channel-value p))
				   (cyco-value-error 'meta-channel name)))))
	   
    (defmethod meta-channel ((name symbol) &optional (resolve t))
      (if resolve
	  (resolve-channel name name 10)
	(resolve-channel-once name)))
    
    (defun channel-name (channel)
      "Returns primary symbolic name for MIDI channel c."
      (let ((channel-index (1- channel)))
	(if (and (>= channel-index 0)(<= channel-index 15))
	    (let ((mc (aref reverse-assignments channel-index)))
	      (meta-channel-name mc))
	  nil)))
    
    (defun ?meta-channels ()
      "Prints list of defined meta channels."
      (let ((acc '()))
	(dolist (p assignments)
	  (push (cons (->string (car p))
		      (->string (meta-channel-value (cdr p)))) acc))
	(dolist (p (sort acc #'(lambda (a b)(string< (car a)(car b)))))
	  (format t "[~16A] --> ~A~%" (car p)(cdr p)))
	(format t "~%")
	(dotimes (channel-index 16)
	  (let* ((chan (1+ channel-index))
		 (primary (channel-name chan)))
	    (format t "[~02D] --> ~A~%" chan primary)))))
    
    (defun meta-channel-assignment-p (object)
      "Predicate, true if object is either a numeric MIDI channel 
or a defined symbolic meta channel."
      (or (and (integerp object)(plusp object)(<= object 16))
	  (assoc object assignments))))) 
