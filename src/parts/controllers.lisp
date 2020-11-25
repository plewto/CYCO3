;;;; **** DEPRECIATED ****


(in-package :cyco-part)

(constant +controllers-properties+
	  (append +part-properties+
		  '(:shift
		    :render-once
		    :curve-function)))

(defclass controllers (part)
  ((states
    ::type list
    :accessor controllers-states
    :initform '())))

(defgeneric controllers-p (object))
(defmethod controllers-p ((object t)) nil)
(defmethod controllers-p ((object controllers)) t)

(let ((previous-1 (->cyco-symbol '*1))
      (previous-2 (->cyco-symbol '*2))
      (argument-count-table (make-hash-table :size 6)))
  (setf (gethash :time argument-count-table) 2
	(gethash :time-to argument-count-table) 1
	(gethash :value argument-count-table) 2
	(gethash :value-to argument-count-table) 1
	(gethash :steps argument-count-table) 1
	(gethash :type argument-count-table) 1)
  (labels ((get-time
	    (state argument)
	    (cond ((eq argument previous-1)
		   (controllers-state-time-start state))
		  ((eq argument previous-2)
		   (controllers-state-time-end state))
		  (t argument)))

	   ;; :time t1 t2
	   (process-time
	    (state clause)
	    (setf (controllers-state-time-start state)(get-time state (second clause))
		  (controllers-state-time-end state)(get-time state (third clause))))
	    
	   ;; time-to t
	   (process-time-to
	    (state clause)
	    (let ((tnew (get-time state (second clause))))
	      (setf (controllers-state-time-start state)(controllers-state-time-end state))
	      (setf (controllers-state-time-end state) tnew)))

	   (get-value
	    (controllers state event clause position)
	    (let ((argument (nth position clause)))
	      (cond ((eq argument previous-1)
		     (controllers-state-value-start state))
		    ((eq argument previous-2)
		     (controllers-state-value-end state))
		    (t (expect-normalized-float controllers event clause :position position :signed t)))))

	   ;; :value v1 v2
	   (process-values
	    (controllers state event clause)
	    (let ((v1 (get-value controllers state event clause 1))
		  (v2 (get-value controllers state event clause 2)))
	      (setf (controllers-state-value-start state) v1)
	      (setf (controllers-state-value-end state) v2)))

	   ;; :value-to v
	   (process-value-to
	    (controllers state event clause)
	    (let ((v (get-value controllers state event clause 1)))
	      (setf (controllers-state-value-start state)(controllers-state-value-end state)
		    (controllers-state-value-end state) v)))

	   ;; :steps n   2 <= n <= 128
	   (process-step-count
	    (controllers state event clause)
	    (setf (controllers-state-steps state)
		  (expect-integer controllers event clause :min 2 :max 128 :default 1)))
			 
	   ;; :type n     n - controller numbner  0 <= n < 128
	   ;;             n - controller name 
	   ;;             n - symbol 'pressure
	   ;;             n - symbol 'bend
	   (process-type
	    (controllers state event clause)
	    (let ((ctype (second clause)))
	      (setf (controllers-state-event-type state)
		    (cond ((eq ctype (->cyco-symbol 'pressure)) :pressure)
			  ((eq ctype (->cyco-symbol 'bend)) :bend)
			  ((and (integerp ctype)(<= 0 ctype)(< ctype 128)) ctype)
			  (t (let ((ctrl (get-controller-number (->cyco-symbol ctype) :default nil)))
			       (if (not ctrl)
				   (progn 
				     (cyco-composition-error
				      (part-id controllers)
				      (clause-id event clause)
				      "Expected controller name or number"
				      (sformat "Encountered ~A" ctype))
				     nil)
				 ctrl)))))))
	   
	   (dispatch-event
	    (controllers state event clause)
	    (let ((command (validate-argument-count controllers event clause argument-count-table)))
	      (cond
	       ((eq command :reset) (reset state))
	       ((eq command :time) (process-time state clause))
	       ((eq command :time-to) (process-time-to state clause))
	       ((eq command :value) (process-values controllers state event clause))
	       ((eq command :value-to) (process-value-to controllers state event clause))
	       ((eq command :steps) (process-step-count controllers state event clause))
	       ((eq command :type) (process-type controllers state event clause))
	       (t
		;; Should never see this
		;; validate-argument-count varifies valid command
		(error (sformat "controllers dispatch-event fallthrough  ~A" event))))))
	   
    (process-events
     (controllers event-list)
     (let ((acc '())
    	   (state (make-controllers-state)))
       (reset state)
       (dolist (event event-list)
    	 (setf (controllers-state-source state) event)
    	 (dolist (clause (partition-list event))
    	   (dispatch-event controllers state event clause)
    	   (if (controllers-state-event-type state)
    	       (progn
    		 (push (clone state) acc)
    		 (soft-reset state)))))
       (reverse acc))) )

  (defun make-controllers (name instruments &key
				section
				cuefn
				shift
				tempo unit bars beats subbeats
				render-once
				(curve #'identity)
				remarks
				events)
    (let* ((part-name (->cyco-symbol name))
	   (parent (validate-section part-name section))
	   (new-part (make-instance 'controllers
				    :properties +controllers-properties+
				    :name part-name
				    :remarks (->string (or remarks ""))
				    :transient t)))
      (put new-part :instruments (->list instruments))
      (put new-part :cue-function cuefn)
      (put new-part :shift (or shift 0.0))
      (put new-part :tempo tempo)
      (put new-part :unit unit)
      (put new-part :bars bars)
      (put new-part :beats beats)
      (put new-part :subbeats subbeats)
      (put new-part :render-once render-once)
      (put new-part :curve-function curve)
      (connect parent new-part)
      (setf (controllers-states new-part)
      	    (process-events new-part (->list events)))
      new-part)) ))

(setf (documentation 'make-controllers 'function) +make-controllers-docstring+)
       
(defmacro controllers (name instruments &key
			    section
			    cuefn
			    shift
			    tempo unit bars beats subbeats
			    render-once
			    (curve #'identity)
			    remarks
			    events)
  `(progn
     (part-banner (name ,section) ',name)
     (let ((new-part (make-controllers ',name ,instruments
				       :section ,section
				       :cuefn ,cuefn
				       :shift ,shift
				       :tempo ,tempo
				       :unit ,unit
				       :bars ,bars
				       :beats ,beats
				       :subbeats ,subbeats
				       :render-once ,render-once
				       :curve ,curve
				       :remarks ,remarks
				       :events ,events)))
       (defparameter ,name new-part)
       new-part)))

(setf (documentation 'controllers 'function) +controllers-docstring+)


(defmethod clone ((source controllers) &key new-parent (new-name "~A"))
  (let* ((name (->cyco-symbol (sformat new-name (name source))))
	 (parent (or new-parent (parent source)))
	 (new-part (make-controllers name (property source :instruments)
				     :section parent
				     :cuefn (property source :cue-function)
				     :shift (property source :shift)
				     :render-once (property source :render-once)
				     :curve (property source :curve-function)
				     :remarks (remarks source)
				     :events nil)))
    (copy-time-signature source new-part)
    (setf (controllers-states new-part)
	  (clone (controllers-states source)))
    (dolist (c (children source))
      (clone c :new-parent new-part))
    new-part))
    
