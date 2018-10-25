;;;; PigIron CYCo sj config quantumleap vop bulgarian
;;;;

(defun vop-bulgarian (&key (parent ql-vop)(channel nil)
			   keynumber-map articulation-map dynamic-map
			   remarks)
  (let ((inst (make-instrument 'vop-bulgarian
			       :parent parent
			       :channel (meta-channel channel)
			       :remarks (or remarks "Range (C0 C9)")
			       :transient t
			       :keynumber-map keynumber-map
			       :articulation-map articulation-map
			       :dynamic-map dynamic-map)))
    (--make-ql-program-map inst '(bul1 
				  bul2 
				  bul3 
				  bul4 
				  hiaheya 
				  melody1 
				  melody2 
				  oho-call-1 
				  oho-call-2 
				  sheep-call 
				  oo 
				  oho 
				  so 
				  woho 
				  ihehoh 
				  oho2 
				  aha 
				  heya 
				  laha))
    (param vop-bulgarian inst)
    inst))


(defun vop-bulgarian-breath (&key (parent ql-vop)(channel nil)
				  keynumber-map articulation-map dynamic-map
				  remarks)
  (let ((inst (instrument vop-bulgarian-breath
			  :parent parent
			  :channel (meta-channel channel)
			  :remarks (or remarks "")
			  :transient t
			  :keynumber-map keynumber-map
			  :articulation-map articulation-map
			  :dynamic-map dynamic-map)))
    (param vop-bulgarian-breath inst)
    inst))


(defun vop-bulgarian-deeper (&key (parent ql-vop)(channel nil)
				  keynumber-map articulation-map dynamic-map
				  remarks)
  (let ((inst (make-instrument 'vop-bulgarian-deeper
			       :parent parent
			       :channel (meta-channel channel)
			       :remarks (or remarks "")
			       :transient t
			       :keynumber-map keynumber-map
			       :articulation-map articulation-map
			       :dynamic-map dynamic-map)))
    (param vop-bulgarian-deeper inst)
    inst))

