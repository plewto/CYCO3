;;;; CYCO parts metronome.lisp
;;;;
;;;; Defines specialized psudo-part METRONOME
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
		  

(defmacro metronome (name &key section tempo unit bars beats cuefn
			  cue key amp dur
			  (instrument *metronome*))
  `(let* ((met (make-metronome ',name
			       :section ,section
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
			
	  
			       
