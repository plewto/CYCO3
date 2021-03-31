;;;; CYCO parts striper.lisp
;;;;

(in-package :cyco-part)


(constant +striper-properties+ (append +part-properties+
				       '(:division)))

(defclass striper (part) nil)
  
(defmethod striper-p ((object striper)) t)

(defun striper (&key name section (division 24))
  (let* ((parent (or section
		     (property *project* :current-section)))
	 (name2 (or name (->symbol (sformat "~A-STRIPER" (name parent)))))
	 (instance (make-instance 'striper :name name2
		   :properties +striper-properties+
		   :transient t
		   :remarks "MIDI clock striper.")))
    (put instance :division division)
    (connect parent instance)
    (reset instance)
    instance))

(defmethod clone ((mother striper) &key new-name new-parent &allow-other-keys)
  (let* ((name (if new-name
		   (sformat new-name (name mother))
		 (name mother)))
	 (parent (or new-parent (parent mother)))
	 (division (property mother :division)))
    (striper :name name :section parent :division division)))

(defmethod render-once ((striper striper) &key (offset 0.0))
  (if (muted-p striper)
      (return-from render-once '()))
  (let* ((quarter-duration (beat-duration (parent striper)))
	 (delta-time (float (/ quarter-duration (property striper :division))))
	 (time offset)
	 (end-time (+ (duration (parent striper)) offset))
	 (events '()))
    (while (< time end-time)
      (push (cons time (midi-clock)) events)
      (setf time (+ time delta-time)))
    (reverse events)))
    
(defmethod render-n ((striper striper)(n integer) &key (offset 0.0))
  (if (muted-p striper)
      (return-from render-n '()))
  (let* ((quarter-duration (beat-duration (parent striper)))
	 (delta-time (float (/ quarter-duration (property striper :division))))
	 (time offset)
	 (end-time (+ offset (* n (duration (parent striper)))))
	 (events '()))
    (while (< time end-time)
      (push (cons time (midi-clock)) events)
      (setf time (+ time delta-time)))
    (reverse events)))
    
    


