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
  
  ;; (defmethod meta-channel ((channel integer) &optional resolve)
  ;;   "In range integer channel numbers map to themselves."
  ;;   (declare (ignore resolve))
  ;;   (if (and (plusp channel)(<= channel 16))
  ;; 	channel
  ;;     (cyco-value-error 'meta-channel channel)))

  (defmethod meta-channel ((channel integer) &key resolve default)
    (declare (ignore resolve default))
    (if (and (plusp channel)(<= channel 16))
	channel
      (cyco-value-error 'meta-channel channel)))
  
  (defmethod meta-channel ((channel null) &key resolve default)
    (declare (ignore resolve default))
    1)

 
  (labels ((resolve-channel (target-name name default depth)
			    (if (zerop depth)
				(progn
				 (cyco-warning (sformat "Unknown MIDI channel ~A, using default ~A." name default))
				 default)
			      (let* ((p (gethash name channel-map))
				     (chan (and p (meta-channel-value p))))
				(if (and (integerp chan)(plusp chan)(<= chan 16))
				    chan
				  (resolve-channel target-name chan default (1- depth))))))
	   
	   (resolve-channel-once (name default)
				 (let ((p (gethash name channel-map)))
				   (or (and p (meta-channel-value p))
				       (progn
					 (cyco-warning (sformat "Unknown MIDI chanhnel ~A, using default ~A." name default))
					 default)))))
	   
    (defmethod meta-channel ((name symbol) &key (resolve t)(default 1))
      (if resolve
	  (resolve-channel name name default 10)
	(resolve-channel-once name default)))

   
    (defun meta-channel-names (channel)
      "Returns list of names for channel."
      (let ((acc (list channel)))
	(maphash #'(lambda (key mchan)
		     (declare (ignore mchan))
		     (let ((chan (meta-channel key :resolve t)))
		       (if (= chan channel)
			   (push key acc))))
		 channel-map)
	acc))

    (defun ?meta-channels ()
      "Display assigned Meta channels."
      (dolist (c (range 1 17))
	(dolist (n (meta-channel-names c))
	  (if (not (eq c n))
	      (format t "[MIDI channel ~2d] ~A~%" c n)))))
    
    (defun meta-channel-assignment-p (object)
      "Predicate, true if object is either a numeric MIDI channel 
or a defined symbolic meta channel."
      (or (and (integerp object)(plusp object)(<= object 16))
	  (gethash object channel-map))) ))
