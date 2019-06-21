;;;; CYCO plugins ion korg microsampler
;;;;

(instrument microsampler
	    :parent *ROOT-INSTRUMENT*
	    :transient nil
	    :channel (meta-channel :SAMPLER))

;; minkey and maxkey ignored if keynumber-map specified.
;;
(defmacro microsampler (name &key
			     (parent microsampler)
			     (channel nil)
			     (minkey 0)
			     (maxkey 127)
			     remarks
			     keynumber-map 
			     articulation-map
			     dynamic-map)
  `(let ((inst (make-instrument ',name
				:parent ,parent
				:transient t
				:channel (meta-channel ,channel)
				:remarks (or ,remarks
					     (sformat "Micro Sampler key-range (~A ~A)"
						      ,minkey ,maxkey))
				:articulation-map ,articulation-map
				:dynamic-map ,dynamic-map
				:keynumber-map (or ,keynumber-map 
						   (basic-keynumber-map :min ,minkey :max ,maxkey)))))
     (defparameter ,name inst)
     inst))
