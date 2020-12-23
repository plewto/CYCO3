;;;; CYCO parts strummer.lisp
;;;;
;;;; A STRUMMER generates MIDI events from an events-list
;;;; with particular attention to how chords are produced.
;;;; See SIMPLE-PART for a version with less overhead/features.
;;;;

(in-package :cyco-part)

(constant +strummer-properties+ +part-properties+)


(defclass strummer (part)
  ((states				
    :type list
    :accessor strummer-states
    :initform '())))

(defgeneric strummer-p (object))
(defmethod strummer-p ((object t)) nil)
(defmethod strummer-p ((strummer strummer)) t)

(let ((argument-count-table (make-hash-table :size 25)))
  (setf (gethash :reset argument-count-table) 0
	(gethash :time argument-count-table) 1
	(gethash :chord argument-count-table) 1
	(gethash :inversion argument-count-table) 1
	(gethash :inv argument-count-table) 1
	(gethash :octave argument-count-table) 1
	(gethash :oct argument-count-table) 1
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
  (labels ((real-event-p
	    (state)
	    (or (strummer-state-key state)
		(strummer-state-controller-value state)
		(strummer-state-bend state)
		(strummer-state-program-number state)
		(strummer-state-program-bank state)
		(strummer-state-grace-key state)))

	   ;; :time time-specification
	   (process-time 
	    (strummer state clause)
	    (let* ((cue-function (property strummer :cue-function))
		   (shuffle-function (property strummer :shuffle-function))
		   (time-specification (second clause))
		   (time (+ (funcall cue-function strummer time-specification)
			    (funcall shuffle-function time-specification))))
	      (setf (strummer-state-time-specification state) time-specification
		    (strummer-state-time state) time)))
	   
	   (use-chord-by-name
	    (strummer state chord-name)
	    (let ((chord-model (property strummer :chord-model))
		  (cname (->cyco-symbol chord-name)))
	      (if (defines-chord-p chord-model cname)
		  (progn 
		    (setf (strummer-state-chord-type state) cname)
		    t)
		nil)))
	   
	   (use-chord-by-template
	    (state template-list)
	    (setf (strummer-state-chord-type state) template-list)
	    t)
    
	   (process-chord
	    (strummer state event clause)
	    (let ((chord-specification (second clause)))
	      (if (listp chord-specification)
		  (use-chord-by-template state chord-specification)
		(if (not (use-chord-by-name strummer state chord-specification))
		    (cyco-composition-error
		     (part-id strummer)
		     (clause-id event clause)
		     (sformat "~A chord-model does not defines ~A"
			      (name (property strummer :chord-model))
			      chord-specification))))))
	   
	   ;; :inversion n    -12 <= n <= +12
	   (process-chord-inversion 
	    (strummer state event clause)
	    (let ((value (expect-integer strummer event clause :min -12 :max 12)))
	      (setf (strummer-state-chord-inversion state) value)))

	   ;; :octave n   -3 <= n <= +3
	   (process-chord-octave 
	    (strummer state event clause)
	    (let ((value (expect-integer strummer event clause :min -3 :max 3)))
	      (setf (strummer-state-chord-octave state) value)))

	   (use-metric-expression
	    (strummer relative-time)
	    (* relative-time (/ 60.0 (tempo strummer))))

	   (get-articulation 
	    (strummer event clause &key (default 0.0))
	    (let ((expression (second clause)))
	      (if (numberp expression)
	   	  (float expression)
	   	(let ((relative-time (metric-expression-p expression)))
	   	  (if relative-time
	   	      (use-metric-expression strummer relative-time)
	   	    (progn
	   	      (cyco-composition-error
	   	       (part-id strummer)
	   	       (clause-id event clause)
	   	       "Expected metric-expression or time"
	   	       (sformat "Encountered ~A" expression)
	   	       (sformat "using default ~A" default))
	   	      default))))))

	   ;; :strum delay   delay is either as metric expression or number.
	   (process-strum-delay
	    (strummer state event clause)
	    (let ((delay (get-articulation strummer event clause)))
	      (setf (strummer-state-strum-delay state) delay)))

	   ;; :grace-duration articulation  (metric expression or time)
	   (process-grace-articulation
	    (strummer state event clause)
	    (let ((delay (get-articulation strummer event clause)))
	      (setf (strummer-state-grace-articulation state) delay)))

	   ;; :grace-delay delay   (metric expression or time)
	   ;;                      use -1*metric-expression to move event before
	   ;;                      current time.
	   (process-grace-delay 
	    (strummer state event clause)
	    (let ((delay (get-articulation strummer event clause)))
	      (setf (strummer-state-grace-delay state) delay)))

	   ;; :dur articulation  (metric expression or time)
	   (process-note-articulation
	    (strummer state event clause)
	    (let ((delay (get-articulation strummer event clause)))
	      (setf (strummer-state-articulation state) delay)))

	   ;; :strum* acceleration      0.1 <= acceleration <= 8.0
	   (process-strum-acceleration
	    (strummer state event clause)
	    (let ((acceleration (expect-float strummer event clause :min 0.1 :max 8.0 :default 1.0)))
	      (setf (strummer-state-strum-acceleration state) acceleration)))

	   ;; :direction d
	   ;; :direction (list...)
	   ;; Valid directions are down up coin and random
	   (process-strum-direction 
	    (strummer state event clause)
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
	      (setf (strummer-state-strum-direction state) (cycle :of (reverse acc)))))

	   ;; :end-together no|yes
	   (process-end-together
	    (strummer state event clause)
	    (let ((flag (expect-yes-no strummer event clause)))
	      (setf (strummer-state-strum-end-together state) flag)))

	   ;; :amp* scale   0.25 <= scale <= 4.0
	   (process-amp-scale 
	       (strummer state event clause)
	     (let ((scale (expect-float strummer event clause :min 0.25 :max 4.0 :default 1.0)))
	       (setf (strummer-state-strum-amp-scale state) scale)))

	   ;; :amp dynamic
	   ;; :amp list     --> converted to cycle
	   (process-dynamics 
	    (strummer state event clause)
	    (let ((dynamic-values (expect-dynamic strummer event clause)))
	      (setf (strummer-state-dynamic state)(cycle :of dynamic-values))))

	   ;; :cres start end count
	   (process-crescendo 
	    (strummer state event clause)
	    (let* ((initial (dynamic (car (expect-dynamic strummer event clause :position 1 :default 0.25))))
		   (final (car (expect-dynamic strummer event clause :position 2 :default 0.75)))
		   (count (or (expect-integer strummer event clause :min 1 :max 128 :position 3 :default 8)
			      8))
		   (diff (float (- final initial)))
		   (delta (/ diff count))
		   (pattern (line :of (append (range initial final :by delta)(list final)))))
	      (setf (strummer-state-dynamic state) pattern)))

	   ;; :amp-limit min max
	   (process-dynamic-limits 
	    (strummer state event clause)
	    (let* ((min (car (expect-dynamic strummer event clause :position 1 :default (dynamic (->cyco-symbol 'pppp)))))
		   (max (car (expect-dynamic strummer event clause :position 2 :default 1.0))))
	      (setf (strummer-state-dynamic-min state) min
		    (strummer-state-dynamic-max state) max)))

	   ;; :amp-blur amount    0.0 <= amount <= 1.0
	   (process-dynamic-blur 
	    (strummer state event clause)
	    (let ((depth (expect-float strummer event clause :min 0.0 :max 1.0 :default 0.0)))
	      (setf (strummer-state-dynamic-blur state) depth)))

	   ;; :grace-amp* scale   0.1 <= n <= 4.0
	   (process-grace-dynamic
	    (strummer state event clause)
	    (let ((scale (expect-float strummer event clause :min 0.1 :max 4.0 :default 1.0)))
	      (setf (strummer-state-grace-amp-scale state) scale)))

	   (convert-keynumber 
	    (strummer clause)
	    (let* ((instrument (property strummer :instruments))
		   (source-keynumber (second clause))
		   (converted-keynumber (funcall (keynumber-map instrument) source-keynumber)))
	      (if (rest-p converted-keynumber)
		  nil
		converted-keynumber)))

	   ;; :key keynumber
	   (process-keynumber 
	    (strummer state clause)
	    (let ((keynumber (convert-keynumber strummer clause)))
	      (if keynumber
		  (setf (strummer-state-key state) keynumber))))
	   
	   ;; :grace keynumber
	   (process-grace-key 
	    (strummer state clause)
	    (let ((keynumber (convert-keynumber strummer clause)))
	      (if keynumber
		  (setf (strummer-state-grace-key state) keynumber))))
	   
	   ;; :bend amount   -1.0 <= amount <= +1.0
	   (process-bend
	    (strummer state event clause)
	    (let ((value (expect-normalized-float strummer event clause :signed t)))
	      (setf (strummer-state-bend state) value)))

	   ;; :cc ctrl value    0 <= ctrl < 128,  0 <= value < 128
	   (process-controller 
	    (strummer state event clause)
	    (let ((controller-number (get-controller-number (second clause) :default 1))
		  (value (expect-integer strummer event clause :position 2 :min 0 :max 127 :default 0)))
	      (setf (strummer-state-controller-number state) (or controller-number 0)
		    (strummer-state-controller-value state) value)))

	   (default->keyword 
	     (value)
	     (if (eq (->cyco-symbol value)(->cyco-symbol 'default))
		 :default
	       value))

	   ;; :program p   p is un-evaluated until render time
	   (process-program 
	    (state clause)
	    (let ((value (default->keyword (second clause))))
	      (setf (strummer-state-program-number state) value)))

	   ;; :bank bank program  (un-evaluated)
	   (process-bank-and-program 
	    (state clause)
	    (let ((bank (default->keyword (second clause)))
	   	  (program (default->keyword (third clause))))
	      (setf (strummer-state-program-bank state) bank)
	      (setf (strummer-state-program-number state) program)))
	   
	   (dispatch-event-clause
	    (strummer state event clause)
	    (setf (strummer-state-source state) event)
	    (let ((command (validate-argument-count strummer event clause argument-count-table)))
	      (cond
	       ((eq command :reset) (reset state))
	       ((eq command :time) (process-time strummer state clause))
	       ((eq command :chord) (process-chord strummer state event clause))
	       ((or (eq command :inversion)(eq command :inv))
		(process-chord-inversion strummer state event clause))
	       ((or (eq command :octave)(eq command :oct))
		(process-chord-octave strummer state event clause))
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
		(error "strummer-event dispatch fall through.")))))
	   
	   (process-states 
	    (strummer event-list)
	    (let ((acc '())
		  (state (make-strummer-state)))
	      (reset state)
	      (dolist (event event-list)
		(dolist (clause (partition-list event))
		  (dispatch-event-clause strummer state event clause))
		(if (real-event-p state)
		    (progn
		      (push (clone state) acc)
		      (soft-reset state))))
	      (setf (strummer-states strummer)(reverse acc)))) )
    
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
	(connect parent new-strummer)
	(put new-strummer :instruments instrument)
	(put new-strummer :tempo tempo)
	(put new-strummer :unit unit)
	(put new-strummer :bars bars)
	(put new-strummer :beats beats)
	(put new-strummer :subbeats subbeats)
	(init-time-signature new-strummer)
	(put new-strummer :cue-function cuefn)
	(put new-strummer :shuffle-function shuffle)
	(put new-strummer :render-once render-once)
	(put new-strummer :transposable transposable)
	(put new-strummer :reversible reversible)
	(put new-strummer :chord-model chord-model)
	(put new-strummer :muted nil)
	(put new-strummer :shift (scale-time-parameter (or shift 0) new-strummer))
	(setf (strummer-states new-strummer)
	      (process-states new-strummer (->list events)))
	new-strummer)) ))

(setf (documentation 'make-strummer 'function) +make-strummer-docstring+)

(defmacro strummer (name instrument &key
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
  `(progn
     (part-banner (name ,section) ',name)
     (let ((new-strummer (make-strummer ',name ,instrument
					:section ,section
					:cuefn ,cuefn
					:shuffle ,shuffle
					:shift  ,shift 
					:tempo  ,tempo 
					:unit  ,unit 
					:bars  ,bars 
					:beats  ,beats 
					:subbeats ,subbeats
					:render-once ,render-once
					:transposable ,transposable
					:reversible ,reversible
					:chord-model ,chord-model
					:remarks ,remarks
					:events ,events)))
       (defparameter ,name new-strummer)
       new-strummer)))
					
(setf (documentation 'strummer 'function) +strummer-docstring+)

(defmethod clone ((mother strummer) &key new-parent new-name)
  (let* ((name (->cyco-symbol (sformat (or new-name "~A")(name mother))))
	 (parent (or new-parent (parent mother)))
	 (daughter (make-strummer name (property mother :instruments)
				  :section parent
				  :cuefn (property mother :cue-function)
				  :shuffle (property mother :shuffle-function)
				  :transposable (property mother :transposable)
				  :reversible (property mother :reversible)
				  :chord-model (property mother :chord-model)
				  :render-once (property mother :render-once)
				  :shift (property mother :shift)
				  :remarks (remarks mother)
				  :events '())))
    (copy-part-properties mother daughter)
    (copy-time-signature mother daughter)
    (setf (strummer-states daughter)
	  (clone (strummer-states mother)))
    (reset daughter)
    daughter))
