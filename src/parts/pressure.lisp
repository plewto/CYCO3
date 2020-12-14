;;;; CYCO parts touch.lisp
;;;;

(in-package :cyco-part)

(constant +touch-properties+
	  (append +part-properties+ '(:no-thin)))


(defclass touch (part)
  ((backing-generator
    :type controllers
    :accessor touch-backing-generator
    :initform (make-controllers 'touch-generator 1))))
    
    

(defgeneric touch-p (item))
(defmethod touch-p ((item t)) nil)
(defmethod touch-p ((item touch)) t)


(defun make-touch (name instruments &key
			   section
			   cuefn
			   shuffle
			   shift
			   tempo unit bars beats subbeats
			   render-once
			   remarks
			   no-thin
			   events)
  (let* ((part-name (->cyco-symbol name))
	 (parent (validate-section part-name section))
	 (part (make-instance 'touch
			      :properties +controllers-properties+
			      :name part-name
			      :remarks (->string (or remarks ""))
			      :transient t)))
    (connect parent part)
    (put part :instruments (->list instruments))
    (put part :cue-function cuefn)
    (put part :shuffle-function shuffle)
    (put part :shift (scale-time-parameter (or shift 0) part))
    (put part :tempo tempo)
    (put part :unit unit)
    (put part :bars bars)
    (put part :beats beats)
    (put part :subbeats subbeats)
    (init-time-signature part)
    (put part :render-once render-once)
    (put part :transposable nil)
    (put part :reversible nil)
    (put part :no-thin no-thin)
    (push '(:ctrl 1) events)
    (setf (touch-backing-generator part)
	  (make-controllers 'touch-generator instruments
			    :section part
			    :events events))
    (reset part)
    part))

(defmacro touch (name instruments &key
			 section
			 cuefn
			 shuffle
			 shift
			 tempo unit bars beats subbeats
			 render-once
			 remarks
			 no-thin
			 events)
  `(progn
     (part-banner (name ,section) ',name)
     (let ((part (make-touch ',name ,instruments
				:section ,section
				:cuefn ,cuefn
				:shuffle ,shuffle
				:shift ,shift
				:tempo  ,tempo 
				:unit  ,unit 
				:bars  ,bars 
				:beats  ,beats 
				:subbeats ,subbeats
				:render-once ,render-once
				:no-thin ,no-thin
				:remarks ,remarks
				:events ,events)))
       (defparameter ,name part)
       part)))

(defmethod clone ((mother touch) &key new-name new-parent)
  (let* ((frmt (or new-name "~A"))
	 (name (sformat frmt (name mother)))
	 (parent (or new-parent (parent mother)))
	 (daughter (make-touch name
			       (property mother :instruments)
			       :section parent
			       :cuefn (property mother :cue-function)
			       :shuffle (property mother :shuffle)
			       :shift (property mother :shift)
			       :tempo  (property mother :tempo )
			       :unit  (property mother :unit )
			       :bars  (property mother :bars )
			       :beats  (property mother :beats )
			       :subbeats (property mother :subbeats)
			       :render-once (property mother :render-once)
			       :no-thin (property mother :no-thin)
			       :remarks (property mother :remarks)
			       :events (clone (property mother :events)))))
    (copy-part-properties mother daughter)
    (copy-time-signature mother daughter)
    (copy-time-signature daughter (touch-backing-generator daughter))
    daughter))


(defmethod reset ((part touch))
  (reset (touch-backing-generator part))
  part)


(defmethod render-once ((part touch) &key (offset 0.0))
  (if (muted-p part)(return-from render-once '()))
  (reset part)
  (let ((acc '())
	(template (render-once (touch-backing-generator part) :offset offset)))
    (dolist (event template)
      (let* ((time (car event))
	     (message (cdr event))
	     (ci (channel-index message))
	     (value (data message 1)))
	(push (cons time (midi-channel-pressure ci value)) acc)))
    (sort-midi-events acc)))

(defmethod render-n ((part touch)(n integer) &key (offset 0.0))
  (let* ((period (phrase-duration part))
	 (midi-events '())
	 (template (render-once part :offset offset)))
    (dotimes (i (if (property part :render-once) 1 n))
      (dolist (event template)
	(let ((relative-time (car event))
	      (message (cdr event)))
	  (push (cons (+ (* i period) relative-time)(clone message))
		midi-events))))
    (sort-midi-events midi-events)))
