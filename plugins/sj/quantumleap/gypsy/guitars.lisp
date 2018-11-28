;;;; PigIron CYCO sj config quantumleap gypsy guitars
;;;;
;;;; Instruments
;;;;
;;;;     classical-guitar
;;;;     django 
;;;;     django-chords 
;;;;     flamenco 
;;;;     flamenco-chords 
;;;;     spanish-guitar
;;;;


(defun classical-guitar (&key (parent ql-guitar)(channel nil)
			      keynumber-map articulation-map dynamic-map
			      remarks)
  (let ((inst (make-instrument 'classical-guitar
			       :parent parent
			       :channel (meta-channel channel)
			       :remarks (or remarks "Gypsy Guitar")
			       :transient t
			       :keynumber-map keynumber-map
			       :articulation-map articulation-map
			       :dynamic-map dynamic-map)))
    (--make-ql-program-map inst
			   '((sus        "sus        :range (E1 A4)" )
			     (sus-vib    "sus-vib    :range (E1 A4)" )
			     (legato     "legato     :range (E1 A4)" )
			     (harmonics  "harmonics  :range (E1 e5)" )
			     (mute-strum "mute-strum :range (E1 A4)" )
			     (fret-noise "fret-noise :range (E1 C6)" )))
    (defparameter classical-guitar inst)
    inst))


(defun django (&key (parent ql-guitar)(channel nil)
		    keynumber-map articulation-map dynamic-map
		    remarks)
  (let ((inst (make-instrument 'django
			       :parent parent
			       :channel (meta-channel channel)
			       :remarks (or remarks "Gypsy Guitar")
			       :transient t
			       :keynumber-map keynumber-map
			       :articulation-map articulation-map
			       :dynamic-map dynamic-map)))
    (--make-ql-program-map inst
			   '((short "short :range (E1 D5)")
			     (long  "long :range (E1 D5)" )))
    (defparameter django inst)
    inst))

(defun django-chords (&key (parent ql-guitar)(channel nil)
			   keynumber-map articulation-map dynamic-map
			   remarks)
  (let ((inst (make-instrument 'django-chords
			       :parent parent
			       :channel (meta-channel channel)
			       :remarks (or remarks "Gypsy Guitar")
			       :transient t
			       :keynumber-map keynumber-map
			       :articulation-map articulation-map
			       :dynamic-map dynamic-map)))
    (--make-ql-program-map inst
			   '((maj           "major     :range (E1 D5)")
			     (min           "minor     :range (E1 D5)")
			     (dom7          "dom7      :range (E1 D5)")
			     (maj9          "maj9      :range (E1 D5)")
			     (dim7          "dim7      :range (E1 D5)")
			     (maj6          "maj6      :range (E1 D5)")
			     (min7          "min7      :range (E1 D5)")
			     (maj7          "maj7      :range (E1 D5)")
			     (seven-flat-5  "seven-flat-5 :range (E1 D5)")))
    (defparameter django-chords inst)
    inst))


(defun flamenco (&key (parent ql-guitar)(channel nil)
		      keynumber-map articulation-map dynamic-map
		      remarks)
  (let ((inst (make-instrument 'flamenco
			       :parent parent
			       :channel (meta-channel channel)
			       :remarks (or remarks "Gypsy Guitar")
			       :transient t
			       :keynumber-map keynumber-map
			       :articulation-map articulation-map
			       :dynamic-map dynamic-map)))
    (--make-ql-program-map inst
			   '((sus         "Sustain        :range (E1 A4)")
			     (sus-bridge  "Sustain-bridge :range (E1 A4)")
			     (trem        "Tremolo        :range (E1 D4)")
			     (taps        "Taps           :range (D2 C6)")))
    (defparameter flamenco inst)
    inst))


(defun flamenco-chords (&key (parent ql-guitar)(channel nil)
			     keynumber-map articulation-map dynamic-map
			     remarks)
  (let ((inst (make-instrument 'flamenco-chords
			       :parent parent
			       :channel (meta-channel channel)
			       :remarks (or remarks "Gypsy Guitar")
			       :transient t
			       :keynumber-map keynumber-map
			       :articulation-map articulation-map
			       :dynamic-map dynamic-map)))
    (--make-ql-program-map inst
			   '((fast-major    "fast-major")
			     (fast-minor    "fast-minor")
			     (arpeg-major   "arpeg-major")
			     (arpeg-minor   "arpeg-minor")
			     (trem-major    "trem-major")
			     (trem-minor    "trem-minor")
			     (roll-major    "roll-major")
			     (roll-minor    "roll-minor")
			     (double-major  "double-major")
			     (double-minor  "double-minor")
			     (sus-major     "sus-major")
			     (sus-minor     "sus-minor")
			     (sus-vs-maj    "sus-vs-maj")
			     (sus-vs-min    "sus-vs-min")
			     (stacato-maj   "stacato-maj")
			     (stacato-min   "stacato-min")))
    (defparameter flamenco-chords inst)
    inst))
