;;;; PigIron CYCO sj config quantumleap vop welsh
;;;;

(defun vop-welsh (&key (parent ql-vop)(channel nil)
			  keynumber-map articulation-map dynamic-map
			  remarks)
  (let ((inst (make-instrument 'vop-welsh
			       :parent parent
			       :channel (meta-channel channel)
			       :remarks (or remarks "")
			       :transient t
			       :keynumber-map keynumber-map
			       :articulation-map articulation-map
			       :dynamic-map dynamic-map)))
    (--make-ql-program-map inst '(AH OH EE OO MM))
    (defparameter vop-welsh inst)
    inst))


(defun vop-welsh-ah (&key (parent ql-vop)(channel nil)
			  keynumber-map articulation-map dynamic-map
			  remarks)
  (let ((inst (make-instrument 'vop-welsh-ah
			       :parent parent
			       :channel (meta-channel channel)
			       :remarks (or remarks "")
			       :transient t
			       :keynumber-map keynumber-map
			       :articulation-map articulation-map
			       :dynamic-map dynamic-map)))
    (defparameter vop-welsh-ah inst)
    inst))


(defun vop-welsh-oh (&key (parent ql-vop)(channel nil)
			  keynumber-map articulation-map dynamic-map
			  remarks)
  (let ((inst (make-instrument 'vop-welsh-oh
			       :parent parent
			       :channel (meta-channel channel)
			       :remarks (or remarks "")
			       :transient t
			       :keynumber-map keynumber-map
			       :articulation-map articulation-map
			       :dynamic-map dynamic-map)))
    (defparameter vop-welsh-oh inst)
    inst))

(defun vop-welsh-words (&key (parent ql-vop)(channel nil)
			     keynumber-map articulation-map dynamic-map
			     remarks)
  (let ((inst (make-instrument 'vop-welsh-words
			       :parent parent
			       :channel (meta-channel channel)
			       :remarks (or remarks "Range (C0 C9)")
			       :transient t
			       :keynumber-map keynumber-map
			       :articulation-map articulation-map
			       :dynamic-map dynamic-map)))
    (--make-ql-program-map inst '(bene breath close dark death domini 
				       dream drown im fall fire fly gaia 
				       grass hasan hate how in len love 
				       luxet ly mei ness of ooze pray preist 
				       row ruins run san sing so soft this 
				       true uram ventius ver vosh fortuna 
				       from gravis is rain the))
    (defparameter vop-welsh-words inst)
    inst))

