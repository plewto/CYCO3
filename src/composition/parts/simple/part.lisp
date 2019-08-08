;;;; CYCO
;;;; simple-part simple-part
;;;;

(defclass simple-part (part)
  ((events
    :type list ; of simple-state
    :accessor simple-part-events
    :initform '())))

(defgeneric simple-part-p (item))
(defmethod simple-part-p ((item t)) nil)
(defmethod simple-part-p ((item simple-part)) t)

(defun validate-section (part-name section)
  (or (and (section-p section) section)
      (and (project-p *project*)
	   (property *project* :current-section))
      (cyco-composition-error
       'make-simple-part
       (sformat "No default project or section while creating simple-part ~A" part-name))))

(defun part-id (part)
  (sformat "Section ~A  Part ~A" (name (parent part))(name part)))

(defun expect-error (part event clause expected encountered)
  (cyco-composition-error
   'make-simple-part
   (part-id part)
   (sformat "Event  : ~A" event)
   (sformat "Clause : ~A" clause)
   (sformat "Expected    : ~A" expected)
   (sformat "Encountered : ~A" encountered)))

(defun expect-1-argument (part event clause)
  (let ((argument-count (1- (length clause))))
    (if (= argument-count 1)
	t
      (expect-error part event clause "1 argument" argument-count))))

(defun expect-2-arguments (part event clause)
  (let ((argument-count (1- (length clause))))
    (if (= argument-count 2)
	t
      (expect-error part event clause "2 arguments" argument-count))))

(defun expect-integer (part event clause &key (min -1e6)(max 1e6)(default 0))
  (let ((value (second clause)))
    (if (and (integerp value)
	     (<= min value)
	     (<= value max))
	value
      (progn 
	(expect-error part event clause
		      (sformat "Integer between ~A and ~A" min max)
		      (sformat "~A,   Using default ~A" value default))
	default))))

(defun expect-normalized-value (part event clause position)
  (let ((value (nth position clause)))
    (if (and (numberp value)(<= 0 value)(<= value 1))
	(float value)
      (progn 
	(expect-error part event clause
		      "Normalized float"
		      (sformat "~A,  using default 0.0" value))
	0.0))))

(defun expect-signed-normalized-value (part event clause)
  (let ((value (second clause)))
    (if (and (numberp value)(<= -1 value)(<= value +1))
	(float value)
      (progn
	(expect-error part event clause
		      "Signed normalized float"
		      (sformat "~A,   using default 0.0" value))
	0.0))))

;; :time time-specification
(defun process-time (part state event clause)
  (if (expect-1-argument part event clause)
      (let* ((cuefn (property part :cue-function))
	     (shuffle (property part :shuffle-function))
	     (time-specification (second clause))
	     (time (+ (funcall cuefn part time-specification)
	     	      (funcall shuffle time-specification))))
	(setf (simple-state-time-specification state) time-specification
	      (simple-state-time state) time))))

;; :key keynumber  (unevaluated)
(defun process-key (part state event clause)
  (if (expect-1-argument part event clause)
      (setf (simple-state-key state)(second clause))))

;; :chord name
;; :chord template
(flet ((process-chord-by-name (part state chord-name)
			      (let ((chord-model (property part :chord-model)))
				(if (defines-chord-p chord-model (->cyco-symbol chord-name))
				    (setf (simple-state-chord-type state) chord-name)
				  (cyco-composition-error
				   'make-simple-part
				   (part-id part)
				   (sformat "Chord model ~A does not define chord ~A"
					    (name chord-model) chord-name)))))
       (process-chord-as-template (state template)
			       (setf (simple-state-chord-type state) template)))
  (defun process-chord (part state event clause)
    (if (expect-1-argument part event clause)
	(let ((chord-specification (second clause)))
	  (if (listp chord-specification)
	      (process-chord-as-template state chord-specification)
	    (process-chord-by-name part state chord-specification))))))


;; :inversion n    -12 <= n <= +12
(defun process-chord-inversion (part state event clause)
  (if (expect-1-argument part event clause)
      (let ((value (expect-integer part event clause :min -12 :max 12 :default 0)))
	(setf (simple-state-chord-inversion state) value))))

;; :octave n   -3 <= n <= +3
(defun process-chord-octave (part state event clause)
  (if (expect-1-argument part event clause)
      (let ((value (expect-integer part event clause :min -3 :max 3 :default 0)))
	(setf (simple-state-chord-octave state) value))))

;; :dur metric-expression
(defun process-articulation (part state event clause)
  (if (and (expect-1-argument part event clause)
	   (metric-expression-p (second clause)))
      (let* ((metric-expression (second clause))
	     (delay (if (numberp metric-expression)
			(abs (float metric-expression))
		      (* (metric-expression metric-expression)
			 (/ 60.0 (tempo part))))))
	(setf (simple-state-articulation state) delay))
    (expect-error part event clause
		  "metric-expression"
		  (sformat "~A" (second clause)))))

;; :amp dynamic (unevaluated)
(defun process-dynamic (part state event clause)
  (if (expect-1-argument part event clause)
      (setf (simple-state-dynamic state) (second clause))))

;; :pressure value
(defun process-pressure (part state event clause)
  (setf (simple-state-pressure state)(expect-normalized-value part event clause 1)))

;; :cc  controller-number value
(defun process-controller (part state event clause)
  (if (expect-2-arguments part event clause)
      (let ((controller (expect-integer part event clause :min 0 :max 127 :default 0))
	    (value (expect-normalized-value part event clause 2)))
	(setf (simple-state-controller-number state) controller
	      (simple-state-controller-value state) value))))
;; :bend value
(defun process-bend (part state event clause)
  (if (expect-1-argument part event clause)
      (setf (simple-state-bend state)
	    (expect-signed-normalized-value part event clause))))

;; :program number (unevaluated)
(defun process-simple-program-change (part state event clause)
  (if (expect-1-argument part event clause)
      (setf (simple-state-program-number state)(second clause))))

;; :bank bank-number program-number (unevaluated)
(defun process-bank-and-program (part state event clause)
  (if (expect-2-arguments part event clause)
      (setf (simple-state-program-bank state) (second clause)
	    (simple-state-program-number state) (third clause))))

(defun dispatch-event (part state event clause)
  (let ((command (car clause)))
    (cond
     ((eq command :reset)
            (reset state))
     ((eq command :time)
      (process-time part state event clause))
     ((eq command :key)
      (process-key part state event clause))
     ((eq command :chord)
      (process-chord part state event clause))
     ((eq command :inversion)
      (process-chord-inversion part state event clause))
     ((eq command :octave)
      (process-chord-octave part state event clause))
     ((eq command :dur)
      (process-articulation part state event clause))
     ((eq command :amp)
      (process-dynamic part state event clause))
     ((eq command :pressure)
      (process-pressure part state event clause))
     ((eq command :cc)
      (process-controller part state event clause))
     ((eq command :bend)
      (process-bend part state event clause))
     ((eq command :program)
      (process-simple-program-change part state event clause))
     ((eq command :bank)
      (process-bank-and-program part state event clause))
     (t (cyco-composition-error
	 'make-simple-part
	 (sformat "Invalid simple-part ~A event: ~A"
		  (name part) command))))))

(defun real-event-p (state)
  (or (simple-state-key state)
      (simple-state-pressure state)
      (and (simple-state-controller-number state)
	   (simple-state-controller-value state))
      (simple-state-bend state)
      (simple-state-program-number state)))

(defun process-events (simple-part event-list)
  (let ((acc '())
	(state (make-simple-state)))
    (reset state)
    (dolist (event event-list)
      (setf (simple-state-source state) event)
      (dolist (clause (partition-list event))
	(dispatch-event simple-part state event clause))
      (if (real-event-p state)
	  (progn 
	    (push (clone state) acc)
	    (soft-reset state))))
    (reverse acc)))
  
(defun make-simple-part (name instruments &key
			      section
			      cuefn
			      shuffle
			      shift
			      tempo unit bars beats subbeats
			      render-once
			      transposable
			      reversible
			      chord-model
			      remarks
			      events)
  (let* ((part-name (->cyco-symbol name))
	 (parent (validate-section part-name section))
	 (new-part (make-instance 'simple-part
				  :properties +simple-part-properties+
				  :name part-name
				  :remarks (->string (or remarks ""))
				  :transient t)))
    (put new-part :instruments (->list instruments))
    (put new-part :cue-function cuefn)
    (put new-part :shuffle-function shuffle)
    (put new-part :shift (or shift 0.0))
    (put new-part :tempo tempo)
    (put new-part :unit unit)
    (put new-part :bars bars)
    (put new-part :beats beats)
    (put new-part :subbeats subbeats)
    (put new-part :render-once render-once)
    (put new-part :transposable transposable)
    (put new-part :reversible reversible)
    (put new-part :chord-model chord-model)
    (connect parent new-part)
    (set-cyco-prompt)
    (setf (simple-part-events new-part)
    	  (process-events new-part (->list events)))
    new-part))

(setf (documentation 'make-simple-part 'function) +docs+)


(defmethod dump ((part simple-part))
  (format t "SIMPLE-PART ~A~%" (name part))
  (dolist (state (simple-part-events part))
    (format t "~A~%" (->string state)))
  nil)


(defmethod transpose ((part simple-part)(x t))
  (if (property part :transposable)
      (dolist (state (simple-part-events part))
	(transpose state x)))
  part)

(defmethod invert ((part simple-part)(pivot t))
  (if (and pivot (property part :transposable))
      (dolist (state (simple-part-events part))
	(invert state pivot)))
  part)


(defmethod retrograde ((part simple-part))
  (if (property part :reversible)
      (let ((acc '()))
	(dolist (state (simple-part-events part))
	  (let ((kn (simple-state-key state)))
	    (if kn (push kn acc))))
	(dolist (state (simple-part-events part))
	  (let ((kn (simple-state-key state)))
	    (if kn (setf (simple-state-key state)(pop acc)))))))
  part)


(defmethod clone ((source simple-part) &key new-parent new-name)
  (let* ((name (->cyco-symbol (sformat (or new-name "~A") (name source))))
	 (parent (or new-parent (parent source)))
	 (new-part (make-simple-part name (property source :instruments)
				     :section parent
				     :cuefn (property source :cue-function)
				     :shuffle (property source :shuffle-function)
				     :transposable (property source :transposable)
				     :reversible (property source :reversible)
				     :chord-model (property source :chord-model)
				     :remarks (property source :remarks)
				     :events '())))
    (copy-time-signature source new-part)
    (let ((acc '()))
      (dolist (state (simple-part-events source))
	(push (clone state) acc))
      (setf (simple-part-events new-part)(reverse acc)))
    (dolist (c (children source))
      (clone c :new-parent new-part))
    new-part))
      
	      
