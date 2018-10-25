;;;; PigIron cyco orchestra channel-assingments
;;;; Defines nested symbolic MIDI channel name assignments.
;;;;
;;;; (meta-channel nil)    --> nil
;;;; (meta-channel nil resolve) --> 0

;; Returns MIDI channel assigned to key.
;;
(defgeneric meta-channel (key  &optional resolve))

(defstruct meta-channel
  name
  value
  remarks)

(let ((assignments '())
      (reverse-assignments (->vector (range 1 17))))

  (defun reset-channel-assignments ()
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

  (defun .meta-channel. (target-name name depth)
    (if (zerop depth)
  	(cyco-circular-assignment-error 'meta-channel! target-name)
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
    (let ((ci (1- c)))
      (if (and (>= ci 0)(<= ci 15))
	  (let ((mc (aref reverse-assignments ci)))
	    (meta-channel-name mc))
	nil)))

  (defun ?meta-channels ()
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
    (or (and (integerp obj)(plusp obj)(<= obj 16))
	(assoc obj assignments))) )
