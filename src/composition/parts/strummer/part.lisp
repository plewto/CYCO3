;;;; cyco-strummer part.lisp

(constant +strummer-properties+
	  (append +part-properties+
		  '(:shift
		    :shuffle-function
		    :render-once)))

(defclass strummer (part)
  ((states				
    :type list
    :accessor strummer-states
    :initform '())))

(defgeneric strummer-p (object))
(defmethod strummer-p ((object t)) nil)
(defmethod strummer-p ((strummer strummer)) t)

(defun validate-section (part-name section)
  (or (and (section-p section) section)
      (and (project-p *project*)
	   (property *project* :current-section))
      (cyco-composition-error
       'make-strummer
       (sformat "No default project or section while creating strummer ~A" part-name))))

(defun part-id (part)
  (sformat "Section ~A  Part ~A" (name (parent part))(name part)))

(defun expect-integer (strummer event clause &key (position 1)(min -1e9)(max 1e9)(default 0))
  (let ((value (nth position clause)))
    (if (and (integerp value)(<= min value)(<= value max))
	value
      (progn 
	(cyco-composition-error
	 'make-strummer
	 (part-id strummer)
	 (sformat "Expected integer between ~A and ~A" min max)
	 (sformat "Encountered ~A" value)
	 (sformat "Using default ~A" default)
	 (sformat "Event  : ~A" event)
	 (sformat "Clause : ~A" clause))
	 default))))

(defun expect-float (strummer event clause &key (position 1)(min -1e9)(max 1e9)(default 0.0))
  (let ((value (nth position clause)))
    (if (and (numberp value)(<= min value)(<= value max))
	(float value)
      (progn
	(cyco-composition-error
	 'make-strummer
	 (part-id strummer)
	 (sformat "Expected float between ~A and ~A" min max)
	 (sformat "Encountered ~A" value)
	 (sformat "Using default ~A" default)
	 (sformat "Event  : ~A" event)
	 (sformat "Clause : ~A" clause))
	 default))))

(defun expect-normalized-float (strummer event clause &key (position 1)(signed nil))
  (let ((default 0.0)
	(value (nth position clause)))
    (if (and (numberp value)(<= (if signed -1 0) value)(<= value 1))
	(float value)
      (progn
	(cyco-composition-error
	 'make-strummer
	 (part-id strummer)
	 (sformat "Expected normalized float between ~A and 1.0" (if signed "-1.0" "0.0"))
	 (sformat "Encountered ~A" value)
	 (sformat "Using default ~A" default)
	 (sformat "Event  : ~A" event)
	 (sformat "Clause : ~A" clause))
	 default))))

(let* ((false (->cyco-symbol 'no))
       (true (->cyco-symbol 'yes))
       (default true)
       (default-value t))
  (defun expect-yes-no (strummer event clause)
    (let ((value (second clause)))
      (cond ((eq value false) nil)
	    ((eq value true) t)
	    (t
	     (cyco-composition-error
	      'make-strummer
	      (part-id strummer)
	      "Expected yes or no"
	      (sformat "Encountered ~A" value)
	      (sformat "Event   ~A" event)
	      (sformat "Clause  ~A" clause)
	      (sformat "Using default ~A" default))
	     default-value)))))
    
(let ((argument-count-table (make-hash-table :size 25)))
  (setf (gethash :reset argument-count-table) 0
	(gethash :time argument-count-table) 1
	(gethash :chord argument-count-table) 1
	(gethash :inversion argument-count-table) 1
	(gethash :octave argument-count-table) 1
	(gethash :strum argument-count-table) 1
	(gethash :strum* argument-count-table) 1
	(gethash :direction argument-count-table) 1
	(gethash :amp* argument-count-table) 1
	(gethash :end-together argument-count-table) 1
	(gethash :grace-amp* argument-count-table) 1
	(gethash :grace-duration argument-count-table) 1
	(gethash :grace-delay argument-count-table) 1
	(gethash :dur argument-count-table) 1
	(gethash :amp argument-count-table) 1
	(gethash :cres argument-count-table) 3
	(gethash :amp-blur argument-count-table) 1
	(gethash :amp-limits argument-count-table) 2
	(gethash :key argument-count-table) 1
	(gethash :grace argument-count-table) 1
	(gethash :bend argument-count-table) 1
	(gethash :cc argument-count-table) 2
	(gethash :program argument-count-table) 1
	(gethash :bank argument-count-table) 2)
  
  (defun validate-argument-count (strummer event clause command)
    (let ((count (1- (length clause)))
	  (expected (gethash command argument-count-table)))
      (if (not expected)
	  (progn 
	    (cyco-composition-error
	     'make-strummer
	     (part-id strummer)
	     (sformat "Invalid event clause: ~A" command)
	     (sformat "Event  : ~A" event)
	     (sformat "Clause : ~A" clause))
	    (return-from validate-argument-count nil))
	(if (not (= expected count))
	    (progn 
	      (cyco-composition-error
	       'make-strummer
	       (part-id strummer)
	       (sformat "Expected ~A argument~A" expected (if (= expected 1) "" "s"))
	       (sformat "Encountered ~A" count)
	       (sformat "Event  : ~A" event)
	       (sformat "Clause : ~A" clause))
	      (return-from validate-argument-count nil))))
      t)))

;; :time time-specification
(defun process-time (strummer state clause)
  (let* ((cue-function (property strummer :cue-function))
	 (shuffle-function (property strummer :shuffle-function))
	 (time-specification (second clause))
	 (time (+ (funcall cue-function strummer time-specification)
		  (funcall shuffle-function time-specification))))
    (setf (state-time-specification state) time-specification
	  (state-time state) time)))

;; :chord name
;; :chord template-list
(labels ((by-name (strummer state chord-name)
		  (let ((chord-model (property strummer :chord-model))
			(cname (->cyco-symbol chord-name)))
		    (if (defines-chord-p chord-model cname)
			(setf (state-chord-type state) cname)
		      (cyco-composition-error
		       'make-strummer
		       (part-id strummer)
		       (sformat "Chord Model ~A does not define chord ~A" (name chord-model) cname)))))
	 
	 (by-list (state template-list)
		  (setf (state-chord-type state) template-list)))

  (defun process-chord (strummer state clause)
    (let ((chord-specification (second clause)))
      (if (listp chord-specification)
    	  (by-list state chord-specification)
    	(by-name strummer state chord-specification)))))

;; :inversion n    -12 <= n <= +12
(defun process-chord-inversion (strummer state event clause)
  (let ((value (expect-integer strummer event clause :min -12 :max 12)))
    (setf (state-chord-inversion state) value)))
	
;; :octave n   -3 <= n <= +3
(defun process-chord-octave (strummer state event clause)
  (let ((value (expect-integer strummer event clause :min -3 :max 3)))
    (setf (state-chord-octave state) value)))

(labels ((use-metric-expression (strummer relative-time)
				(* relative-time (/ 60.0 (tempo strummer))))
	 (get-articulation (strummer event clause &key (default 0.0))
	 		   (let ((expression (second clause)))
	 		     (if (numberp expression)
	 			 (float expression)
	 		       (let ((relative-time (metric-expression-p expression)))
	 			 (if relative-time
	 			     (use-metric-expression strummer relative-time)
	 			   (progn
	 			     (cyco-composition-error
	 			       'make-strummer
	 			       (part-id strummer)
	 			       "Expected metric-expression or time"
	 			       (sformat "Encountered ~A" expression)
	 			       (sformat "event   : ~A" event)
	 			       (sformat "clause  : ~A" clause)
	 			       (sformat "using default ~A" default))
	 			     default)))))))

  ;; :strum delay   delay is either as metric expression or number.
  (defun process-strum-delay (strummer state event clause)
    (let ((delay (get-articulation strummer event clause)))
      (setf (state-strum-delay state) delay)))

  ;; :grace-duration articulation  (metric expression or time)
  (defun process-grace-articulation (strummer state event clause)
    (let ((delay (get-articulation strummer event clause)))
      (setf (state-grace-articulation state) delay)))

  ;; :grace-delay delay   (metric expression or time)
  ;;                      use -1*metric-expression to move event before
  ;;                      current time.
  (defun process-grace-delay (strummer state event clause)
    (let ((delay (get-articulation strummer event clause)))
      (setf (state-grace-delay state) delay)))

  ;; :dur articulation  (metric expression or time)
  (defun process-note-articulation (strummer state event clause)
    (let ((delay (get-articulation strummer event clause)))
      (setf (state-articulation state) delay))) )
				       
;; :strum* acceleration      0.1 <= acceleration <= 8.0
(defun process-strum-acceleration (strummer state event clause)
  (let ((acceleration (expect-float strummer event clause :min 0.1 :max 8.0 :default 1.0)))
    (setf (state-strum-acceleration state) acceleration)))

;; :direction d
;; :direction (list...)
;; Valid directions are down up coin and random
(defun process-strum-direction (strummer state event clause)
  (let ((acc '())
	(directions (->list (second clause))))
    (dolist (direction-name (mapcar #'(lambda (q)(symbol-name q)) directions))
      (cond ((string= direction-name "DOWN")(push :down acc))
	    ((string= direction-name "UP")(push :up acc))
	    ((string= direction-name "DICE")(push :dice acc))
	    ((string= direction-name "RANDOM")(push :random acc))
	    (t (cyco-composition-error
		'make-strummer
		(part-id strummer)
		(sformat "Invalid strum direction:  ~A" direction-name)
		(sformat "Expected one of DOWN UP DICE or RANDOM")
		(sformat "Event  : ~A" event)
		(sformat "Clause : ~A" clause)
		"Using default DOWN")
	       (push :down acc))))
    (setf (state-strum-direction state) (cycle :of (reverse acc)))))

;; :end-together no|yes
(defun process-end-together (strummer state event clause)
  (let ((flag (expect-yes-no strummer event clause)))
    (setf (state-strum-end-together state) flag)))

;; :amp* scale   0.25 <= scale <= 4.0
(defun process-amp-scale (strummer state event clause)
  (let ((scale (expect-float strummer event clause :min 0.25 :max 4.0 :default 1.0)))
    (setf (state-strum-amp-scale state) scale)))

(labels ((expect-dynamic (strummer event clause &key (position 1)(default 0.5))
			 (let ((values (->list (nth position clause))))
			   (if (not (every #'dynamic-p values))
			       (progn
				 (cyco-composition-error
				  'make-strummer
				  (part-id strummer)
				  "Expected dynamic values"
				  (sformat "Encountered ~A" values)
				  (sformat "Event  ~A" event)
				  (sformat "Clause ~A" clause)
				  (sformat "Using default ~A" default))
				 (->list default))
			     (dynamic values)))))

  ;; :amp dynamic
  ;; :amp list     --> converted to cycle
  (defun process-dynamics (strummer state event clause)
    (let ((dynamic-values (expect-dynamic strummer event clause)))
      (setf (state-dynamic state)(cycle :of dynamic-values))))

  ;; :cres start end count
  (defun process-crescendo (strummer state event clause)
    (let* ((initial (dynamic (car (expect-dynamic strummer event clause :position 1 :default 0.25))))
    	   (final (car (expect-dynamic strummer event clause :position 2 :default 0.75)))
    	   (count (or (expect-integer strummer event clause :min 1 :max 128 :position 3 :default 8)
		      8))
    	   (diff (float (- final initial)))
    	   (delta (/ diff count))
    	   (pattern (line :of (append (range initial final :by delta)(list final)))))
      (setf (state-dynamic state) pattern)))

  ;; :amp-limit min max
  (defun process-dynamic-limits (strummer state event clause)
    (let* ((min (car (expect-dynamic strummer event clause :position 1 :default (dynamic (->cyco-symbol 'pppp)))))
	   (max (car (expect-dynamic strummer event clause :position 2 :default 1.0))))
      (setf (state-dynamic-min state) min
	    (state-dynamic-max state) max))) )

;; :amp-blur amount    0.0 <= amount <= 1.0
(defun process-dynamic-blur (strummer state event clause)
  (let ((depth (expect-float strummer event clause :min 0.0 :max 1.0 :default 0.0)))
    (setf (state-dynamic-blur state) depth)))

;; :grace-amp* scale   0.1 <= n <= 4.0
(defun process-grace-dynamic (strummer state event clause)
  (let ((scale (expect-float strummer event clause :min 0.1 :max 4.0 :default 1.0)))
    (setf (state-grace-amp-scale state) scale)))			

(labels ((convert-keynumber (strummer clause)
			    (let* ((instrument (property strummer :instruments))
				   (source-keynumber (second clause))
				   (converted-keynumber (funcall (keynumber-map instrument) source-keynumber)))
			      (if (rest-p converted-keynumber)
				  nil
				converted-keynumber))))
  ;; :key keynumber
  (defun process-keynumber (strummer state clause)
    (let ((keynumber (convert-keynumber strummer clause)))
      (if keynumber
  	  (setf (state-key state) keynumber))))

  ;; :grace keynumber
  (defun process-grace-key (strummer state clause)
    (let ((keynumber (convert-keynumber strummer clause)))
      (if keynumber
  	  (setf (state-grace-key state) keynumber)))) )

;; :bend amount   -1.0 <= amount <= +1.0
(defun process-bend (strummer state event clause)
  (let ((value (expect-normalized-float strummer event clause :signed t)))
    (setf (state-bend state) value)))

;; :cc ctrl value    0 <= ctrl <= 127,  0.0 <= value <= 1.0
(defun process-controller (strummer state event clause)
  (let ((controller-number (expect-integer strummer event clause :position 1 :min 0 :max 127 :default 0))
	(value (expect-normalized-float strummer event clause :position 2)))
    (setf (state-controller-number state) (or controller-number 0)
	  (state-controller-value state) value)))

(labels ((default->keyword (value)
	   (if (eq (->cyco-symbol value) (->cyco-symbol 'default))
	       :default
	     value)))

  ;; :program p   p is un-evaluated until render time
  (defun process-program (state clause)
    (let ((value (default->keyword (second clause))))
      (setf (state-program-number state) value)))

  ;; :bank bank program  (un-evaluated)
  (defun process-bank-and-program (state clause)
    (let ((bank (default->keyword (second clause)))
	  (program (default->keyword (third clause))))
      (setf (state-program-bank state) bank)
      (setf (state-program-number state) program))))

(defun dispatch-event-clause (strummer state event clause)
  (let ((command (car clause)))
    (setf (state-source state) event)
    (if (validate-argument-count strummer event clause command)
	(cond
	 ((eq command :reset) (reset state))
	 ((eq command :time) (process-time strummer state clause))
	 ((eq command :chord) (process-chord strummer state clause))
	 ((eq command :inversion) (process-chord-inversion strummer state event clause))
	 ((eq command :octave) (process-chord-octave strummer state event clause))
	 ((eq command :strum) (process-strum-delay strummer state event clause))
	 ((eq command :strum*) (process-strum-acceleration strummer state event clause))
	 ((eq command :direction) (process-strum-direction strummer state event clause))
	 ((eq command :end-together) (process-end-together strummer state event clause))
	 ((eq command :amp*) (process-amp-scale strummer state event clause))
	 ((eq command :grace-amp*) (process-grace-dynamic strummer state event clause))
	 ((eq command :grace-duration) (process-grace-articulation strummer state event clause))
	 ((eq command :grace-delay) (process-grace-delay strummer state event clause))
	 ((eq command :dur) (process-note-articulation strummer state event clause))
	 ((eq command :amp) (process-dynamics strummer state event clause))
	 ((eq command :cres) (process-crescendo strummer state event clause))
	 ((eq command :amp-blur) (process-dynamic-blur strummer state event clause))
	 ((eq command :amp-limits) (process-dynamic-limits strummer state event clause))
	 ((eq command :key) (process-keynumber strummer state clause))
	 ((eq command :grace) (process-grace-key strummer state clause))
	 ((eq command :bend) (process-bend strummer state event clause))
	 ((eq command :cc) (process-controller strummer state event clause))
	 ((eq command :program) (process-program state clause))
	 ((eq command :bank) (process-bank-and-program state clause))
	 (t
	  ;; Should never see this.
	  ;; validate-argument-count catches invalid commands.
	  (error "strummer-event dispatch fall through."))))))

(defun process-states (strummer event-list)
  (let ((acc '())
	(state (make-state)))
    (reset state)
    (dolist (event event-list)
      (dolist (clause (partition-list event))
	(dispatch-event-clause strummer state event clause))
      (if (real-event-p state)
	  (progn
	    (push (clone state) acc)
	    (soft-reset state))))
    (setf (strummer-states strummer)(reverse acc))))

	  
(defun make-strummer (name instrument &key
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
	 (new-strummer (make-instance 'strummer
				      :properties +strummer-properties+
				      :name part-name
				      :remarks (->string (or remarks ""))
				      :transient t)))
    (put new-strummer :instruments instrument)
    (put new-strummer :tempo tempo)
    (put new-strummer :unit unit)
    (put new-strummer :bars bars)
    (put new-strummer :beats beats)
    (put new-strummer :subbeats subbeats)
    (put new-strummer :cue-function cuefn)
    (put new-strummer :shuffle-function shuffle)
    (put new-strummer :render-once render-once)
    (put new-strummer :transposable transposable)
    (put new-strummer :reversible reversible)
    (put new-strummer :chord-model chord-model)
    (put new-strummer :muted nil)
    (put new-strummer :shift (or shift 0.0))
    (connect parent new-strummer)
    (setf (strummer-states new-strummer)
    	  (process-states new-strummer (->list events)))
    new-strummer))
