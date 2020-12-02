;;;; CYCO3 composition/project/render-mode
;;;; Defines order of Sections within a project and the manner in which
;;;; they are rendered.


(defstruct section-render-mode
  section-name
  shift ;; time offset in seconds
  count ;; <= 0 --> skip
  transpose
  invert
  retrograde)
  
(defmethod clone ((mother section-render-mode) &key new-name new-parent)
  (dismiss new-name new-parent)
  (make-section-render-mode
   :section-name (section-render-mode-section-name mother)
   :shift (section-render-mode-shift mother)
   :count (section-render-mode-count mother)
   :retrograde (section-render-mode-retrograde mother)
   :transpose (section-render-mode-transpose mother)
   :invert (section-render-mode-invert mother)))


(defmethod section-order  ((render-mode section-render-mode) &key (project *project*))
  (let ((section-name (section-render-mode-section-name render-mode)))
    (if (find-child project section-name)
	(put project :section-order
	     (reverse (cons render-mode (reverse (property project :section-order)))))
      (cyco-composition-error 'section-order
			      (sformat "Section ~A does not exists" section-name)))))
(defmethod section-order ((section-name symbol) &key (project *project*))
  "Adds named section to the project section-order list.
All modifiers have default values, shift=0.0, count=1, transpose=0, invert=nil, retrograde=nil."
  (if (find-child project section-name)
      (let ((render-mode (make-section-render-mode :section-name section-name
						   :shift 0.0
						   :count 1
						   :transpose 0
						   :invert nil
						   :retrograde nil)))
	(section-order render-mode project))
    (cyco-composition-error 'section-order
			    (sformat "Section ~A does not exists" section-name))))
  

(labels ((type-error (spec expected offending)
		     (cyco-type-error 'project.section-order expected offending
				      (sformat "Invalid section-order specification: ~A" spec)))

	 (validate (mode-specification section-name count transpose invert)
		   (if (not (symbolp section-name))
		       (type-error mode-specification 'symbol section-name))
		   (if (not (integerp count))
		       (type-error mode-specification 'integer count))
		   (if (not (integerp transpose))
		       (type-error mode-specification 'integer transpose))
		   (if (not (or (null invert)(keynumber-p invert)))
		       (type-error mode-specification 'keynumber invert))))  
  

  (defmethod section-order ((section-name-list list) &key (project *project*))
    "Sets order of sections within project.
Elements of section-name-list may either be a simple section-name or
a list of form:

     (section-name :x n :trans tx :invert kn :retro b :shift time)

All elements except section-name are optional.  The additional clauses
modify how the section is rendered.

    :x n        - repeat n times, default 1.  If less then 1 the section is skipped.
    :trans tx   - transpose amount in half-steps, default 0.
    :invert kn  - key inversion pivot-key, default nil.
    :retro flag - reverse events, default nil
    :shift time - time offset in seconds, default 0.0.

:trans and :invert are ignored by sections/parts whose :transposable property is nil.
:retro is ignored by sections/parts whose :reversible property is nil."
    
    (dolist (mode-specification section-name-list)
      (let* ((spec (->list mode-specification))
	     (section-name (car spec))
	     (count (or (second (member :x spec)) 1))
	     (shift (or (second (member :shift spec)) 0.0))
	     (retro (or (second (member :retro spec)) nil))
	     (transpose (or (second (member :trans spec)) 0))
	     (invert (second (member :invert spec))))
	(validate mode-specification section-name count transpose invert)
	(section-order (make-section-render-mode :section-name section-name
						 :shift (float shift)
						 :count count
						 :transpose transpose
						 :retrograde retro
						 :invert (if invert (keynumber invert) nil))
		       :project project)))))


(defun clear-section-order (&optional (project *project*))
  "Removes all section-order information from project."
  (put project :section-order '()))
