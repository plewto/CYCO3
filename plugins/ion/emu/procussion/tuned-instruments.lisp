;;;; CYCO plugins ion emu procussion tuned-instruments.lisp
;;;;
;;;;  Instruments:
;;;;      vibrations
;;;;      marimba
;;;;      rosewood
;;;;      malletbells
;;;;      indo-steel
;;;;      killer-synth
;;;;      mystic-land
;;;;      clavarimba 


(defun vibrations (&key (parent PROB)(channel nil) articulation-map dynamic-map)
  (instrument vibrations
	      :parent parent
	      :program (procussion-program 'vibrations)
	      :channel channel
	      :articulation-map articulation-map
	      :dynamic-map dynamic-map
	      :transient t))

(defun marimba (&key (parent PROB)(channel nil) articulation-map dynamic-map)
  (instrument marimba
	      :parent parent
	      :program (procussion-program 'marimba)
	      :channel channel
	      :articulation-map articulation-map
	      :dynamic-map dynamic-map
	      :transient t))

(defun rosewood (&key (parent PROB)(channel nil) articulation-map dynamic-map)
  (instrument rosewood
	      :parent parent
	      :program (procussion-program 'rosewood)
	      :channel channel
	      :articulation-map articulation-map
	      :dynamic-map dynamic-map
	      :transient t))

(defun malletbells (&key (parent PROB)(channel nil) articulation-map dynamic-map)
  (instrument malletbells
	      :parent parent
	      :program (procussion-program 'malletbells)
	      :channel channel
	      :articulation-map articulation-map
	      :dynamic-map dynamic-map
	      :transient t))

(defun indo-steel (&key (parent PROB)(channel nil) articulation-map dynamic-map)
  (instrument indo-steel
	      :parent parent
	      :program (procussion-program 'indo-steel)
	      :channel channel
	      :articulation-map articulation-map
	      :dynamic-map dynamic-map
	      :transient t))

(defun killer-synth (&key (parent PROB)(channel nil) articulation-map dynamic-map)
  (instrument killer-synth
	      :parent parent
	      :program (procussion-program 'killer-synth)
	      :channel channel
	      :articulation-map articulation-map
	      :dynamic-map dynamic-map
	      :transient t))

(defun mystic-land (&key (parent PROB)(channel nil) articulation-map dynamic-map)
  (instrument mystic-land
	      :parent parent
	      :program (procussion-program 'mystic-land)
	      :channel channel
	      :articulation-map articulation-map
	      :dynamic-map dynamic-map
	      :transient t))

(defun clavarimba (&key (parent PROB)(channel nil) articulation-map dynamic-map)
  (instrument clavarimba
	      :parent parent
	      :program (procussion-program 'clavarimba)
	      :channel channel
	      :articulation-map articulation-map
	      :dynamic-map dynamic-map
	      :transient t))
