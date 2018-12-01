;;;; PigIron CYCO sj config korg R3
;;;;

(let ((bank-alist '((A  0)(B  1)(C  2)(D  3)
		    (E  4)(F  5)(G  6)(H  7)
		    (I  8)(J  9)(K 10)(L 11)
		    (M 12)(N 13)(O 14)(P 15))))
  (flet ((r3-bank
	  (bank)
	  (if (and (numberp bank)(<= 1 bank)(<= bank 16))
	      (1- bank)
	    (or (second (assoc bank bank-alist))
		(cyco-warning
		 (sformat "IllegaL R3 Bank number: ~A" bank)
		 nil))))
	 (warnfn
	  (bank program)
	  (cyco-warning
	   (sformat "Illegal R3 program  bank: ~A  program: ~A" bank program)
	   "Using default   bank: A  program: 1")))

    (defun r3-program (bank program)
      (let ((b (r3-bank bank)))
	(if (not b)
	    (progn 
	      (warnfn bank program)
	      (return-from r3-program 0))
	  (let ((p (+ (* 8 b)(1- program))))
	    (if (and (<= 0 p)(< p 128))
		p
	      (warnfn bank program))))))))

(instrument korg-r3
	    :parent +root-instrument+
	    :transient nil
	    :channel (meta-channel :R3)
	    :remarks "Korg R3 parent instrument")

(defmacro r3 (name bank program &key
		   (parent korg-r3)
		   (channel nil)
		   remarks
		   keynumber-map
		   articulation-map
		   dynamic-map)
  `(let* ((rem (or ,remarks
		   (sformat "Korg R3  program ~A.~A" ',bank ,program)))
	  (inst (make-instrument ',name
				:parent ,parent
				:program (r3-program ',bank ,program)
				:channel (meta-channel ,channel)
				:remarks (->string rem)
				:transient t
				:keynumber-map ,keynumber-map
				:articulation-map ,articulation-map
				:dynamic-map ,dynamic-map)))
     (defparameter ,name inst)
     inst))
		