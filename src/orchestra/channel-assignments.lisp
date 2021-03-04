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

(let ((channel-map (make-hash-table)))

  (defun reset-channel-assignments ()
    "Clears all meta channel assignments"
    (setf channel-map (make-hash-table))
    (loop for c from 0 to 16
	  do (make-meta-channel
	      :name c
	      :value c
	      :remarks "")))

  (reset-channel-assignments)

  (defun set-meta-channel (name value &optional (remarks ""))
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
      (setf (gethash name channel-map) mchan)
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
				(cyco-error 'set-meta-channel name "Circular MIDI channel assignment")
			      (let* ((p (gethash name channel-map))
				     (chan (and p (meta-channel-value p))))
				(if (and (integerp chan)(plusp chan)(<= chan 16))
				    chan
				  (resolve-channel target-name chan (1- depth))))))
	   (resolve-channel-once (name)
				 (let ((p (gethash name channel-map)))
				   (or (and p (meta-channel-value p))
				       (cyco-value-error 'meta-channel name)))) )
	   
    (defmethod meta-channel ((name symbol) &optional (resolve t))
      (if resolve
	  (resolve-channel name name 10)
	(resolve-channel-once name)))

   
    (defun meta-channel-names (channel)
      "Returns list of names for channel."
      (let ((acc (list channel)))
	(maphash #'(lambda (key mchan)
		     (declare (ignore mchan))
		     (let ((chan (meta-channel key :resolve)))
		       (if (= chan channel)
			   (push key acc))))
		 channel-map)
	acc))
     
    (defun meta-channel-assignment-p (object)
      "Predicate, true if object is either a numeric MIDI channel 
or a defined symbolic meta channel."
      (or (and (integerp object)(plusp object)(<= object 16))
	  (gethash object channel-map))) ))
