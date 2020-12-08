;;;; CYCO parts metronome.lisp
;;;;
;;;; Defines specialized pseudo-part METRONOME
;;;;

(in-package :cyco-part)

(labels ((get-section (section)
		      (cond ((section-p section) section)
			    ((and (project-p *project*)
				  (property *project* :current-section))
			     (property *project* :current-section))
			    (t (cyco-composition-error 'make-metronome
						       "No default Section"))))

	 (make-cue-list (bars beats)
			(let ((acc '()))
			  (dotimes (bar bars)
			    (dotimes (beat beats)
			      (push (list (1+ bar)(1+ beat) 1) acc)))
			  (reverse acc)))
	 
	 (make-key-pattern (bars beats)
			   (let ((acc '()))
			     (dotimes (bar bars)
			       (dotimes (beat beats)
				 (push (cond ((and (zerop beat)(zerop bar)) :PHRASE)
					     ((zerop beat) :BAR)
					     (t :BEAT))
				       acc)))
			     (reverse acc))))

  (defun make-metronome (name &key
			      section
			      shift
			      cuefn
			      tempo unit bars beats
			      cue key amp dur
			      (instrument *metronome*))
    (setf section (get-section section))
    (setf tempo (or tempo (property section :tempo)))
    (setf unit (or unit (property section :unit)))
    (setf bars (or bars (property section :bars)))
    (setf beats (or beats (property section :beats)))
    (let* ((cue-pattern (or cue (make-cue-list bars beats)))
	   (key-pattern (or key (make-key-pattern bars beats)))
	   (amp-pattern (or amp key-pattern))
	   (dur-pattern (or dur key-pattern)))
      (make-qball name instrument
		  :section section
		  :cuefn cuefn
		  :shift shift
		  :tempo tempo
		  :unit unit
		  :bars bars
		  :beats beats
		  :cue cue-pattern
		  :key key-pattern
		  :amp amp-pattern
		  :dur dur-pattern
		  :transposable nil
		  :reset-on-repeat t))) ) 
		  

(defmacro metronome (name &key section tempo unit bars beats cuefn shift
			  cue key amp dur
			  (instrument *metronome*))
  `(let* ((met (make-metronome ',name
			       :section ,section
			       :shift ,shift
			       :tempo ,tempo
			       :unit ,unit
			       :bars ,bars
			       :beats ,beats
			       :cuefn ,cuefn
			       :cue ,cue
			       :key ,key
			       :amp ,amp
			       :dur ,dur
			       :instrument ,instrument)))
     (defparameter ,name met)
     met))


(setf (documentation 'make-metronome 'function)
      	  "Metronome is a specialized part for generating metric ticks.

name        - Symbol
:section    - Parent section, defaults to current section of 8PROJECT*.
:cuefn      - Cuing function, defaults to parent value.
:tempo      - Tempo in BPM, defaults to parent value.
:unit       - Time signature beat unit, defaults to parent value.
:bars       - Time signature bars per phrase, defaults to parent value.
:beats      - Time signature beats per bar, defaults to parent value.
:instrument - Instrument used to generate events, Defaults to *METRONOME*.")


(setf (documentation 'metronome 'function)
      "METRONOME and MAKE-METRONOME are identical except the later binds 
the new part to the symbol name while the later does not.  The name argument
to MAKE-METRONOME should be a quoted symbol and unquoted for METRONOME.")
