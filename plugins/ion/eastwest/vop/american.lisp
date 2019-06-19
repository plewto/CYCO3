;;;; CYCO plugins sj eastwest vop american.lisp
;;;;

(defun vop-american (&key (parent ew-vop)(channel nil)
			  keynumber-map articulation-map dynamic-map
			  remarks)
  (let ((inst (make-instrument 'vop-american
			       :parent parent
			       :channel (meta-channel channel)
			       :remarks (or remarks "Range (C0 C9)")
			       :transient t
			       :keynumber-map keynumber-map
			       :articulation-map articulation-map
			       :dynamic-map dynamic-map)))
    (make-eastwest-program-map inst '(ah 
				      ahoh-vib 
				      ah-vib 
				      ah-short 
				      ah-spooky 
				      ah-staccato 
				      ah-vib 
				      doo 
				      dream 
				      ea-vib 
				      eaah 
				      huoh 
				      mah 
				      mei 
				      maom 
				      mm 
				      muah 
				      oh 
				      oh-opera 
				      oh-vib 
				      ohm 
				      ohm-vib 
				      oo-breathy 
				      oo 
				      oo-vib 
				      ah-rnd 
				      sigh ))
    (param vop-american inst)
    inst))


(defun vop-american-oo (&key (parent ew-vop)(channel nil)
			     keynumber-map articulation-map dynamic-map
			     remarks)
  (let ((inst (instrument vop-american-oo
			  :parent parent
			  :channel (meta-channel channel)
			  :remarks (or remarks "")
			  :transient t
			  :keynumber-map keynumber-map
			  :articulation-map articulation-map
			  :dynamic-map dynamic-map)))
    (param vop-american-oo inst)
    inst))
