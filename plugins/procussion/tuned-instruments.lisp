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
;;;;      thundadome
;;;;      industry
;;;;      churchyard
;;;;      ritual-night
;;;;      intervallix
;;;;      more-basses


(defun vibrations (&key (parent PROCUSSION)(channel nil) articulation-map dynamic-map)
  (instrument vibrations
	      :parent parent
	      :program (procussion-program 'vibrations)
	      :channel channel
	      :articulation-map articulation-map
	      :dynamic-map dynamic-map
	      :transient t))

(defun marimba (&key (parent PROCUSSION)(channel nil) articulation-map dynamic-map)
  (instrument marimba
	      :parent parent
	      :program (procussion-program 'marimba)
	      :channel channel
	      :articulation-map articulation-map
	      :dynamic-map dynamic-map
	      :transient t))

(defun rosewood (&key (parent PROCUSSION)(channel nil) articulation-map dynamic-map)
  (instrument rosewood
	      :parent parent
	      :program (procussion-program 'rosewood)
	      :channel channel
	      :articulation-map articulation-map
	      :dynamic-map dynamic-map
	      :transient t))

(defun malletbells (&key (parent PROCUSSION)(channel nil) articulation-map dynamic-map)
  (instrument malletbells
	      :parent parent
	      :program (procussion-program 'malletbells)
	      :channel channel
	      :articulation-map articulation-map
	      :dynamic-map dynamic-map
	      :transient t))

(defun indo-steel (&key (parent PROCUSSION)(channel nil) articulation-map dynamic-map)
  (instrument indo-steel
	      :parent parent
	      :program (procussion-program 'indo-steel)
	      :channel channel
	      :articulation-map articulation-map
	      :dynamic-map dynamic-map
	      :transient t))

(defun killer-synth (&key (parent PROCUSSION)(channel nil) articulation-map dynamic-map)
  (instrument killer-synth
	      :parent parent
	      :program (procussion-program 'killer-synth)
	      :channel channel
	      :articulation-map articulation-map
	      :dynamic-map dynamic-map
	      :keynumber-map (basic-keynumber-map :min 24 :max 96)
	      :transient t))

(defun mystic-land (&key (parent PROCUSSION)(channel nil) articulation-map dynamic-map)
  (instrument mystic-land
	      :parent parent
	      :program (procussion-program 'mystic-land)
	      :channel channel
	      :articulation-map articulation-map
	      :dynamic-map dynamic-map
	      :keynumber-map (basic-keynumber-map :min 36 :max 96)
	      :transient t))

(defun clavarimba (&key (parent PROCUSSION)(channel nil) articulation-map dynamic-map)
  (instrument clavarimba
	      :parent parent
	      :program (procussion-program 'clavarimba)
	      :channel channel
	      :articulation-map articulation-map
	      :dynamic-map dynamic-map
	      :transient t))
(defun thundadome (&key (parent procussion) (channel nil)  articulation-map dynamic-map)
  (instrument thundadome
	      :parent parent
	      :channel channel
	      :program (procussion-program 'thundadome)
	      :articulation-map articulation-map
	      :dynamic-map dynamic-map
	      :keynumber-map (basic-keynumber-map :min 036 :max 096)
	      :transient t))

(defun industry (&key (parent procussion) (channel nil)  articulation-map dynamic-map)
  (instrument industry
	      :parent parent
	      :channel channel
	      :program (procussion-program 'industry)
	      :articulation-map articulation-map
	      :dynamic-map dynamic-map
	      :keynumber-map (basic-keynumber-map :min 000 :max 127)
	      :transient t))

(defun churchyard (&key (parent procussion) (channel nil)  articulation-map dynamic-map)
  (instrument churchyard
	      :parent parent
	      :channel channel
	      :program (procussion-program 'churchyard)
	      :articulation-map articulation-map
	      :dynamic-map dynamic-map
	      :keynumber-map (basic-keynumber-map :min 036 :max 096)
	      :transient t))

(defun ritual-night (&key (parent procussion) (channel nil)  articulation-map dynamic-map)
  (instrument ritual-night
	      :parent parent
	      :channel channel
	      :program (procussion-program 'ritual-night)
	      :articulation-map articulation-map
	      :dynamic-map dynamic-map
	      :keynumber-map (basic-keynumber-map :min 036 :max 096)
	      :transient t))

(defun intervallix (&key (parent procussion) (channel nil)  articulation-map dynamic-map)
  (instrument intervallix
	      :parent parent
	      :channel channel
	      :program (procussion-program 'intervallix)
	      :articulation-map articulation-map
	      :dynamic-map dynamic-map
	      :keynumber-map (basic-keynumber-map :min 036 :max 096)
	      :transient t))

(defun more-bases (&key (parent procussion) (channel nil)  articulation-map dynamic-map)
  (instrument more-bases
	      :parent parent
	      :channel channel
	      :program (procussion-program 'more-bases)
	      :articulation-map articulation-map
	      :dynamic-map dynamic-map
	      :keynumber-map (basic-keynumber-map :min 036 :max 096)
	      :transient t))
