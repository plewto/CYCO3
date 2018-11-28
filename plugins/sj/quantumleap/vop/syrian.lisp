;;;; PigIron CYCO sj config quantumleap vop indian
;;;;

(defun vop-syrian (&key (parent ql-vop)(channel :VOP)
			  keynumber-map articulation-map dynamic-map
			  remarks)
  (let ((inst (make-instrument 'vop-syrian
			       :parent parent
			       :channel (meta-channel channel)
			       :remarks (or remarks "Range (C0 C9)")
			       :transient t
			       :keynumber-map keynumber-map
			       :articulation-map articulation-map
			       :dynamic-map dynamic-map)))
    (--make-ql-program-map inst  '((c   "key of C   :range (c0 c9)")
				   (cs  "Key of CS  :range (c0 c9)")
				   (d   "Key of D   :range (c0 c9)")
				   (ds  "Key of DS  :range (c0 c9)")
				   (e   "Key of E   :range (c0 c9)")
				   (f   "Key of F   :range (c0 c9)")
				   (fs  "Key of FS  :range (c0 c9)")
				   (g   "Key of G   :range (c0 c9)")
				   (gs  "Key of GS  :range (c0 c9)")
				   (a   "Key of A   :range (c0 c9)")
				   (as  "Key of AS  :range (c0 c9)")
				   (b   "Key of B   :range (c0 c9)")))
    (param vop-syrian inst)
    inst))
