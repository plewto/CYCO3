;;;; PigIron CYCO sj config quantumleap vop indian
;;;;

(defun vop-indian (&key (parent ql-vop)(channel :VOP)
			keynumber-map articulation-map dynamic-map
			remarks)
  (let ((inst (make-instrument 'vop-indian
			       :parent parent
			       :channel (meta-channel channel)
			       :remarks (or remarks "Range (C0 C9)")
			       :transient t
			       :keynumber-map keynumber-map
			       :articulation-map articulation-map
			       :dynamic-map dynamic-map)))
    (--make-ql-program-map inst '(i1 i2 i3 i4 i5
				     i6 i7 i8 i9 i10))
    (param vop-indian inst)
    inst))
