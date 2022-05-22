;;;; cyco parts xball.lisp
;;;;

(in-package :cyco-part)

(constant +xball-properties+
	  (append +part-properties+
		  '(:articulation-pattern
		    :chord-pattern
		    :cue-cycle
		    :direction-pattern
		    :dynamic-pattern
		    :end-together
		    :inversion-pattern
		    :key-pattern
		    :octave-pattern
		    :reset-on-repeat
		    :strum-pattern
		    :end-together
		    )))

(defclass xball (part) nil)

(defgeneric xball-p (object))
(defmethod xball-p ((object t)) nil)
(defmethod xball-p ((object xball)) t)


(labels ((->qpattern (spec)
		    (cond ((pattern-p spec)
			   spec)
			  ((atom spec)
			   (cycle :of (->list spec)))
			  ((pattern-comprehension-p spec)
			   (pattern-comprehension spec))
			  ((listp spec)
			   (cycle :of spec))
			  (t (cyco-error (sformat "Illegal argument to XBALL  ~A  ~A" (type-of spec) spec))))) )

	;; MAY ONLY USE A SINGLE INSTRUMENT
	;;
	(defun make-xball (name instrument &key
				 section cuefn shuffle shift render-once
				 tempo unit bars beats subbeats
				 (transposable nil)
				 (reversible nil)
				 (chord-model *chord-table*)
				 cue
				 (key '(60))
				 (dur '(q))
				 (amp '(mf))
				 (chord '([solo]))
				 (inversion '(0))
				 (octave '(0))
				 (strum '(0.0))
				 (end-together t)
				 (direction '(down))
				 (reset-on-repeat t)
				 remarks)

	  (let* ((parent (or (validate-section name section)
			     (return-from make-xball nil)))
		 (xb (make-instance 'xball
				    :properties +xball-properties+
				    :name name
				    :remarks (->string (or remarks ""))
				    :transient t)))
	    (connect parent xb)
	    (put xb :instruments instrument)
	    (put xb :chord-model chord-model)
	    (put xb :tempo tempo)
	    (put xb :unit unit)
	    (put xb :bars bars)
	    (put xb :beats beats)
	    (put xb :subbeats subbeats)
	    (init-time-signature xb)
	    (put xb :cue-function cuefn)
	    (put xb :shuffle-function shuffle)
	    (put xb :render-once render-once)
	    (put xb :transposable transposable)
	    (put xb :reversible reversible)
	    (put xb :muted nil)
	    (put xb :cue-cycle (->cycle cue))
	    (put xb :key-pattern (->qpattern key))
	    (put xb :articulation-pattern (->qpattern dur))
	    (put xb :dynamic-pattern (->qpattern amp))
	    (put xb :chord-pattern (->qpattern chord))
	    (put xb :inversion-pattern (->qpattern inversion))
	    (put xb :octave-pattern (->qpattern octave))
	    (put xb :strum-pattern (->qpattern strum))
	    (put xb :direction-pattern (->qpattern direction))
	    (put xb :end-together end-together)
	    (put xb :shift (scale-time-parameter (or shift 0) xb))
	    (put xb :reset-on-repeat reset-on-repeat)
	    (validate-render xb)
	    (reset xb)
	    xb)) )

(defmacro xball  (name instrument &key
			section cuefn shuffle shift render-once
			tempo unit bars beats subbeats
			transposable
			reversible
			chord-model
			cue
			key
			dur
			amp
			chord
			inversion
			octave
			strum
			end-together
			direction
			reset-on-repeat
			remarks)
  `(progn
     (part-banner (name ,section) ',name)
     (let ((xb (make-xball ',name ,instrument
			:section ,section 
			:cuefn ,cuefn 
			:shuffle ,shuffle 
			:shift (or ,shift 0.0)
			:render-once ,render-once
			:tempo ,tempo 
			:unit ,unit 
			:bars ,bars 
			:beats ,beats 
			:subbeats ,subbeats
			:transposable ,transposable
			:reversible ,reversible
			:chord-model ,chord-model
			:cue ,cue
			:key (or ,key 60)
			:dur (or ,dur 'q)
			:amp (or ,amp 0.8)
			:chord (or ,chord '[solo])
			:inversion (or ,inversion 0)
			:octave (or ,octave 0)
			:strum (or ,strum 0.0)
			:end-together ,end-together
			:direction (or ,direction 'down)
			:reset-on-repeat ,reset-on-repeat
			:remarks ,remarks)))
       (defparameter ,name xb)
       xb)))


(defmethod transpose ((xb xball)(n t))
  (if (property xb :transposable)
      (put xb :key-pattern
	    (transpose (property xb :key-pattern) n)))
  xb)

(defmethod invert ((xb xball)(pivot t))
  (if (and pivot (property xb :transposable))
      (put xb :key-pattern
	    (invert (property xb :key-pattern)
		    (keynumber pivot))))
  xb)

(defmethod retrograde ((xb xball))
  (if (property xb :reversible)
	(retrograde (property xb :key-pattern)))
  xb)

(defmethod reset ((xb xball))
  (reset (property xb :cue-cycle))
  (reset (property xb :key-pattern))
  (reset (property xb :articulation-pattern))
  (reset (property xb :dynamic-pattern))
  (reset (property xb :cue-cycle))
  (reset (property xb :articulation-pattern))
  (reset (property xb :chord-pattern))
  (reset (property xb :direction-pattern))
  (reset (property xb :dynamic-pattern))
  (reset (property xb :end-together))
  (reset (property xb :inversion-pattern))
  (reset (property xb :key-pattern))
  (reset (property xb :octave-pattern))
  (reset (property xb :strum-pattern))
  xb)

(defmethod soft-reset ((xb xball))
  (reset (property xb :cue-cycle)))

(defmethod clone ((mother xball) &key new-name new-parent)
  (let* ((frmt (or new-name "~A"))
	 (name (->symbol (sformat frmt (name mother))))
	 (parent (or new-parent (parent mother)))
	 (xb (make-xball name (clone (property mother :instruments))
			  :section parent
			  :cuefn (property mother :cue-function)
			  :shuffle (property mother :shuffle-function)
			  :shift (property mother :shift)
			  :transposable (property mother :transposable)
			  :reversible (property mother :reversible)
			  :chord-model (property mother :chord-model)
			  :cue (clone (property mother :cue-cycle))
			  :key (clone (property mother :key-pattern))
			  :dur (clone (property mother :articulation-pattern))
			  :amp (clone (property mother :dynamic-pattern))
			  :chord (clone (property mother :chord-pattern))
			  :inversion (clone (property mother :inversion-pattern))
			  :octave (clone (property mother :octave-pattern))
			  :strum (clone (property mother :strum-pattern))
			  :direction (clone (property mother :direction-pattern))
			  :remarks (remarks mother)
			  :end-together (property mother :end-together))))
    (copy-part-properties mother xb)
    (copy-time-signature mother xb)
    (reset xb)
    xb))
						  
