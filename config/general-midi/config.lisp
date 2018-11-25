;;;; Example CYCO configuration for General MIDI instruments.
;;;;
;;;; These files illustrate a simple configuration scheme based on the
;;;; General MIDI standard.  gm-percussion defines several permanent
;;;; (i.e. non-transient) instruments on MIDI channel 10.  
;;;;
;;;;  +root-instrument+
;;;;     |
;;;;     +-- gm-percussion
;;;;            |
;;;;            +-- gm-cowbell
;;;;            +-- gm-woodblock
;;;;            +-- gm-cymbal
;;;;            +-- gm-drum
;;;;            +-- gm-hihat
;;;;            +-- gm-shaker
;;;;            +-- gm-snare
;;;;            +-- gm-kick
;;;;            +-- gm-cuica
;;;;            +-- gm-timbale
;;;;            +-- gm-tom
;;;;
;;;; These instruments use a symbolic keynumber-map specific to their
;;;; type.   gm-percussion may produce any of the tones associated with
;;;; it's child instruments.  gm-cowbell on the other hand will only produce
;;;; cowbell and other closely related tones (agogo and triangle).
;;;;
;;;;
;;;; The function MAKE-GENERAL-MIDI-INSTRUMENT creates non-percussion
;;;; instruments.  By default these instruments are "transient".  When a
;;;; new project is created all transient instruments are removed from the
;;;; orchestra tree, while non-transient instruments remain in place.
;;;; In general non-transient instruments should only be defined during
;;;; configuration, and all project defined instruments should be
;;;; transient.  This allows a project to be reloaded several times while
;;;; testing without adding needles duplicate instruments to the orchestra.
;;;;

(load-profile-file "gm-program-map")
(load-profile-file "gm-percussion")


(defun make-general-midi-instrument (name &key
					  program 
					  (parent +root-instrument+)
					  (transient t)
					  channel
					  keynumber-map
					  dynamic-map
					  articulation-map
					  remarks)
  "Creates new GENERAL MIDI instrument.
name  - Symbol
:program   - Symbol or integer program-number, defaults to name.
             If an integer is used it must be in range 0,127 inclusive.
             If program is a symbol it must match an entry in 
             +GENERAL-MIDI-PROGRAMS+.  The function ?GENERAL-MIDI-PROGRAMS 
             displays a list of valid program symbols.
:parent    - nil or instance of Instrument, defaults to +ROOT-INSTRUMENT+
:transient - bool, If true this instrument is purged form the orchestra
	     tree by the (PRUNE-ORCHESTRA) function.  Instruments defined
             during configuration are usually non-transient.  Conversely
             instruments created by a project should be transient. Default t.
:channel   - MIDI channel, defaults to parent's channel.
:remarks   - Optional remarks text
:keynumber-map    - See orchestra/keynumber-map.lisp
:dynamic-map      - See orchestra/dynamic-map.lisp
:articulation-map - See orchestra/articulation-map.lisp"

  (let* ((program-number (general-midi-program (or program name)))
	 (inst (make-instrument name
				:parent parent
				:transient transient
				:channel channel
				:keynumber-map keynumber-map
				:dynamic-map dynamic-map
				:articulation-map articulation-map
				:remarks (->string
					  (or remarks
					      (sformat "General MIDI program ~A" program))))))
    (program-number! inst program-number)
    inst))

(defmacro general-midi-instrument (name &key
					program 
					(parent +root-instrument+)
					(transient t)
					channel
					keynumber-map
					dynamic-map
					articulation-map
					remarks)
  "Convenience macro, same as MAKE-GENERAL-MIDI-INSTRUMENT except it binds
the new instrument to the symbol name."
  `(defparameter ,name (make-general-midi-instrument ',name
						     :program ,program
						     :parent ,parent
						     :transient ,transient
						     :channel ,channel
						     :keynumber-map ,keynumber-map
						     :dynamic-map ,dynamic-map
						     :articulation-map ,articulation-map
						     :remarks ,remarks)))
