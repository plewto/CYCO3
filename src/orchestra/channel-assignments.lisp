;;;; CYCO3 src/orchestra/channel-assignments
;;;; Defines nested symbolic MIDI channel names.
;;;;
;;;; (meta-channel nil) --> nil
;;;; (meta-channel nil resolve) --> 1

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

  (defmethod meta-channel ((chan integer) &optional resolve)
    (dismiss resolve)
    (if (and (plusp chan)(<= chan 16))
	chan
      (cyco-value-error 'meta-channel chan)))

  (defmethod meta-channel ((chan null) &optional resolve)
    (if resolve 0 nil))

  ;; ISSUE: Use flet or labels to hide .META-CHANNEL. and .META-CHANNEL-1.
  
  (defun .meta-channel. (target-name name depth)
    (if (zerop depth)
	(cyco-error 'meta-channel! name "Circular channel assignment")
    (let* ((p (cdr (assoc name assignments)))
  	   (chan (and p (meta-channel-value p))))
      (if (and (integerp chan)(plusp chan)(<= chan 16))
  	  chan
  	(.meta-channel. target-name chan (1- depth))))))
  
  (defun .meta-channel-1. (name)
    (let ((p (cdr (assoc name assignments))))
      (or (and p (meta-channel-value p))
  	  (cyco-value-error 'meta-channel name))))
    
  (defmethod meta-channel ((name symbol) &optional (resolve t))
    (if resolve
  	(.meta-channel. name name 5)
      (.meta-channel-1. name)))

  (defun channel-name (c)
    "Returns primary symbolic name for MIDI channel c."
    (let ((ci (1- c)))
      (if (and (>= ci 0)(<= ci 15))
	  (let ((mc (aref reverse-assignments ci)))
	    (meta-channel-name mc))
	nil)))

  (defun ?meta-channels ()
    "print list of defined meta channels."
    (let ((acc '()))
      (dolist (p assignments)
	(push (cons (->string (car p))
		    (->string (meta-channel-value (cdr p)))) acc))
      (dolist (p (sort acc #'(lambda (a b)(string< (car a)(car b)))))
	(format t "[~16A] --> ~A~%" (car p)(cdr p)))
      (format t "~%")
      (dotimes (ci 16)
	(let* ((chan (1+ ci))
	       (primary (channel-name chan)))
	  (format t "[~02D] --> ~A~%" chan primary)))))

  (defun meta-channel-assignment-p (obj)
    "Predicate, true if obj is either a numeric MIDI channel 
or a defined symbolic meta channel."
    (or (and (integerp obj)(plusp obj)(<= obj 16))
	(assoc obj assignments))) )

