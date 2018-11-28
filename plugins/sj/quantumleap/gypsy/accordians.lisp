;;;; PigIron CYCO sj config quantumleap gypsy-accordians
;;;;
;;;; Instruments
;;;;
;;;;     bandoneon
;;;;     campana 
;;;;     excelsior 
;;;;     silvestri 


(let ((accord-alist '((out-full   "out-left-full" )
		      (in-octave  "in-left-octave")
		      (out-single "out-left-single")
		      (in-single  "in-left-single")
		      (out-major  "out-left-major")
		      (out-minor  "out-left-minor")
		      (out-7th    "out-left-7th"))))
  (defun campana (&key (parent ql-gypsy)(channel nil)
		       keynumber-map articulation-map dynamic-map
		       remarks)
    (let ((inst (make-instrument 'campana
		     :parent parent
		     :channel (meta-channel channel)
		     :remarks (or remarks "Gypsy Accordion")
		     :transient t
		     :keynumber-map keynumber-map
		     :articulation-map articulation-map
		     :dynamic-map dynamic-map)))
    (--make-ql-program-map inst accord-alist)
    (param campana inst)
    inst))

  (defun excelsior (&key (parent ql-gypsy)(channel nil)
			 keynumber-map articulation-map dynamic-map
			 remarks)
    (let ((inst (make-instrument 'excelsior
		     :parent parent
		     :channel (meta-channel channel)
		     :remarks (or remarks "Gypsy Accordion")
		     :transient t
		     :keynumber-map keynumber-map
		     :articulation-map articulation-map
		     :dynamic-map dynamic-map)))
    (--make-ql-program-map inst accord-alist)
    (param excelsior inst)
    inst)))

(defun silvestri (&key (parent ql-gypsy)(channel nil)
		       keynumber-map articulation-map dynamic-map
		       remarks)
  (let ((inst (make-instrument 'silvestri
		   :parent parent 
		   :channel (meta-channel channel)
		   :remarks (or remarks "Gypsy Accordion")
		   :transient t
		   :keynumber-map keynumber-map
		   :articulation-map articulation-map
		   :dynamic-map dynamic-map)))
  (--make-ql-program-map inst '((out-full  "out-left-full")
				(in-octave "in-left-octave")
				(out-major "out-left-major")
				(out-minor "out-left-minor")
				(out-7     "out-left-7")))
  (param silvestri inst)
  inst))

(defun bandoneon (&key (parent ql-gypsy)(channel nil)
		       keynumber-map articulation-map dynamic-map
		       remarks)
  (let ((inst (make-instrument 'bandoneon
		   :parent parent 
		   :channel (meta-channel channel)
		   :remarks (or remarks "Gypsy Accordion")
		   :transient t
		   :keynumber-map keynumber-map
		   :articulation-map articulation-map
		   :dynamic-map dynamic-map)))
  (--make-ql-program-map inst '(sus sforzando portato short crescendo 
				    sus-accent-1 sus-accent-2))
  (param bandoneon inst)
  inst))
