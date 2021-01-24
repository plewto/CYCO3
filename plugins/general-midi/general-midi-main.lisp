;;;; CYCO plugins general-midi general-midi-main.lisp
;;;;
;;;; Defines General MIDI instruments.
;;;;
;;;;
;;;; These instruments are divided into two groups: percussion and 
;;;; non-percussion.  The GM-PERCUSSION instrument, and it's descendants,
;;;; are created when the plugin is loaded.  The non-percussion instruments
;;;; are created by calling the GENERAL-MIDI-INSTRUMENT macro.
;;;;
;;;;  *root-instrument*
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
;;;; The percussion instruments use a symbolic keynumber-map specific to
;;;; their type.  However the main instrument, gm-percussion, may produce
;;;; any of the tones associated with it's child instruments.  gm-cowbell
;;;; on the other hand will only produce cowbell and other closely related
;;;; tones (agogo and triangle).
;;;;


(load-plugin-file "gm-program-map")
(load-plugin-file "gm-percussion")

(defun make-general-midi-instrument (name &key
					  program 
					  (parent *root-instrument*)
					  (transient t)
					  channel
					  keynumber-map
					  dynamic-map
					  articulation-map
					  remarks)
  "Creates new GENERAL MIDI instrument.
name       - Symbol
:program   - Symbol or integer program-number, defaults to name.
             If an integer is used it must be in range 0,127 inclusive.
             If program is a symbol it must match an entry in 
             +GENERAL-MIDI-PROGRAMS+.  The function ?GENERAL-MIDI
             displays a list of valid program symbols.
:parent    - nil or instance of Instrument, defaults to *ROOT-INSTRUMENT*
:transient - bool, If true this instrument is purged form the orchestra
	     tree by the (PRUNE-ORCHESTRA) function. Default t
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
    (set-program-number inst program-number)
    inst))

(defmacro general-midi-instrument (name &key
					program 
					(parent *root-instrument*)
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

(defun general-midi-metronome (&key (channel 16)(phrase 84)(bar 72)(beat 67)(program 'woodblock))
  "Creates general-midi metronome instrument and binds it to *METRONOME*"
  (setf *metronome*
	(make-general-midi-instrument
	 'gm-metronome
	 :channel channel
	 :program program
	 :keynumber-map (metronome-keynumber-map :phrase phrase
						 :bar bar
						 :beat beat)
	 :dynamic-map (metronome-dynamic-map :phrase 'fff
					     :beat 'ff
					     :bar 'ff)
	 :articulation-map (metronome-articulation-map))))
