;;;; CYCO   qball.lisp

(in-package :cyco-part)

(constant +qball-properties+
	  (append +part-properties+
		  '(:shift
		    :render-once
		    :cue-cycle
		    :shuffle-function
		    :key-pattern
		    :articulation-pattern
		    :dynamic-pattern
		    :reset-on-repeat)))

(defclass qball (part) nil)

(defgeneric qball-p (object))
(defmethod qball-p ((object t)) nil)
(defmethod qball-p ((object qball)) t)


(labels ((validate-section (part-name section)
			   (cond ((section-p section)
				  section)
				 ((and section (not (section-p section)))
				  (cyco-type-error 'make-qball '(section nil) section)
				  nil)
				 ((not (project-p *project*))
				  (cyco-composition-error
				   'make-qball
				   (sformat "No default project while creating qball ~A" part-name))
				  nil)
				 (t (property *project* :current-section))))
	 
	 ;; Checks that instruments argument is valid.
	 ;;   1) A single instrument      --> converted to list --> 2
	 ;;   2) A list of instruments    --> converted to INSTRUMENT-LAYER pattern.
	 ;;   3) A Pattern of instruments --> Use as is.
	 ;; Returns Pattern.
	 (validate-instruments (part-name instruments)
			       (cond
				((pattern-p instruments)
				 instruments) ;; does not check pattern elements
				((listp instruments)
				 (or (and (every #'instrument-p instruments)
					  (instrument-layer :of instruments))
				     (cyco-type-error 'make-qball "List of instruments"
						      instruments
						      (sformat "part name is ~A" part-name))))
				((instrument-p instruments)
				 (instrument-layer :of (->list instruments)))
				(t (cyco-type-error 'make-qball
						    "Instrument or list of instruments"
						    instruments
						    (sformat "part name is ~A" part-name))))) )
  
  (defun make-qball (name instruments &key
			  section
			  cuefn
			  shuffle 
			  shift
			  tempo unit bars beats subbeats
			  render-once
			  (transposable nil)
			  (reversible nil)
			  cue
			  (key '(60))
			  (dur '(q))
			  (amp '(mf))
			  (reset-on-repeat nil)
			  remarks)
    (let* ((parent (or (validate-section name section)
		       (return-from make-qball nil)))
	   (instrument-pattern (or (validate-instruments name instruments)
				   (return-from make-qball nil)))
	   (new-qball (make-instance 'qball
				     :properties +qball-properties+
				     :name name
				     :remarks (->string (or remarks ""))
				     :transient t)))
      (put new-qball :instruments instrument-pattern)
      (put new-qball :tempo tempo)
      (put new-qball :unit unit)
      (put new-qball :bars bars)
      (put new-qball :beats beats)
      (put new-qball :subbeats subbeats)
      (put new-qball :cue-function cuefn)
      (put new-qball :shuffle-function shuffle)
      (put new-qball :render-once render-once)
      (put new-qball :transposable transposable)
      (put new-qball :reversible reversible)
      (put new-qball :muted nil)
      (put new-qball :cue-cycle (->cycle cue))
      (put new-qball :key-pattern (->pattern (or key '(60))))
      (put new-qball :articulation-pattern (->pattern (or dur 1.0)))
      (put new-qball :dynamic-pattern (->pattern (or amp 0.5)))
      (reset new-qball)
      (put new-qball :reset-on-repeat reset-on-repeat)
      (connect parent new-qball)
      (put new-qball :shift (if shift (float shift) 0.0))
      new-qball))) 

(setf (documentation 'make-qball 'function) +qball-docstring+)

(defmacro qball (name instruments &key
		      section
		      cuefn
		      shuffle
		      shift
		      tempo unit bars beats subbeats
		      render-once
		      transposable
		      reversible
		      cue
  		      key 
  		      dur
  		      amp
  		      reset-on-repeat
  		      remarks)
  `(progn
     (part-banner (name ,section) ',name)
     (let ((new-qball (make-qball ',name ,instruments
			   :section ,section
			   :cuefn ,cuefn
			   :shuffle ,shuffle
			   :shift ,shift
			   :render-once ,render-once
			   :transposable ,transposable
			   :reversible ,reversible
			   :tempo ,tempo
			   :unit ,unit
			   :bars ,bars
			   :beats ,beats
			   :subbeats ,subbeats
			   :cue ,cue
			   :key ,key
			   :dur ,dur
			   :amp ,amp
			   :reset-on-repeat ,reset-on-repeat
			   :remarks ,remarks)))
       (defparameter ,name new-qball)
       new-qball)))

(setf (documentation 'qball 'function)
      (sformat "The QBALL macro is identical to MAKE-QBALL function excpt that it binds the
new object to a symbol named name. ~%~A"
	       +qball-docstring+))
       


(defmethod transpose ((qball qball)(n t))
  (if (property qball :transposable)
      (put qball :key-pattern
	    (transpose (property qball :key-pattern) n)))
  qball)

(defmethod invert ((qball qball)(pivot t))
  (if (and pivot (property qball :transposable))
      (put qball :key-pattern
	    (invert (property qball :key-pattern)
		    (keynumber pivot))))
  qball)

(defmethod retrograde ((qball qball))
  (if (property qball :reversible)
	(retrograde (property qball :key-pattern)))
  qball)

(defmethod reset ((qball qball))
  (reset (property qball :cue-cycle))
  (reset (property qball :key-pattern))
  (reset (property qball :articulation-pattern))
  (reset (property qball :dynamic-pattern))
  qball)

(defmethod soft-reset ((qball qball))
  (reset (property qball :cue-cycle)))

(defmethod clone ((source qball) &key new-name new-parent)
  (let* ((frmt (or new-name "~A"))
	 (name (->symbol (sformat frmt (name source))))
	 (parent (or new-parent (parent source)))
	 (new-qball (make-qball name (clone (property source :instruments))
			  :section parent
			  :cuefn (property source :cue-function)
			  :shuffle (property source :shuffle-function)
			  :shift (property source :shift)
			  :transposable (property source :transposable)
			  :reversible (property source :reversible)
			  :cue (clone (property source :cue-cycle))
			  :key (clone (property source :key-pattern))
			  :dur (clone (property source :articulation-pattern))
			  :amp (clone (property source :dynamic-pattern))
			  :reset-on-repeat (property source :reset-on-repeat)
			  :remarks (remarks source))))
    (copy-time-signature source new-qball)
    (put new-qball :shift (property source :shift))
    (dolist (sub-part (children source))
      (clone sub-part :new-name frmt :new-parent new-qball))
    new-qball))
