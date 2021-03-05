;;;; CYCO examples ex5 orchestra
;;;;

(prune-orchestra)

(general-midi-instrument piano :channel 1)
(general-midi-instrument bass :channel 2)
(general-midi-instrument guitar :channel 3)
(general-midi-instrument strings :channel 4 :program 'string-ensemble)
(general-midi-instrument high-strings :channel 5 :program 'string-ensemble-2
			 :keynumber-map (basic-keynumber-map :transpose 24)
			 :dynamic-map (basic-dynamic-map :scale 0.7))
(general-midi-instrument clarinet :channel 6)

(general-midi-metronome :channel 16 :program 'woodblock
			:phrase 72 :bar 72 :beat 'r)

;; gm drum-kit selection
(set-program-number gm-percussion 0)

