;;;; CYCO examples ex3 orchestra
;;;;

(prune-orchestra)

(param wind-scale 1.00)

(param flute-1-scale  (* wind-scale 1.00))
(param flute-2-scale  (* wind-scale 1.00))
(param oboe-scale     (* wind-scale 1.00))
(param clarinet-scale (* wind-scale 1.00))

(param question-scale 0.60)

(param string-scale 0.7)

(param violin-1-scale (* string-scale 0.40))
(param violin-2-scale (* string-scale 0.40))
(param viola-scale    (* string-scale 0.40))
(param cello-scale    (* string-scale 0.50))
(param ensemble-high-scale (* string-scale 0.40))
(param ensemble-bass-scale (* string-scale 0.60))


(param woodwind-transpose 12)


(general-midi-instrument flute-1 :channel 1 :program 'flute
			 :keynumber-map (basic-keynumber-map :transpose woodwind-transpose)
			 :dynamic-map (basic-dynamic-map :scale flute-1-scale))

(general-midi-instrument flute-2 :channel 2 :program 'flute
			 :keynumber-map (basic-keynumber-map :transpose woodwind-transpose)
			 :dynamic-map (basic-dynamic-map :scale flute-2-scale))

(general-midi-instrument oboe :channel 3
			 :keynumber-map (basic-keynumber-map :transpose woodwind-transpose)
			 :dynamic-map (basic-dynamic-map :scale oboe-scale))

(general-midi-instrument clarinet :channel 4
			 :keynumber-map (basic-keynumber-map :transpose woodwind-transpose)
			 :dynamic-map (basic-dynamic-map :scale clarinet-scale))


(general-midi-instrument question :channel 5 :program 'english-horn
			 :keynumber-map (basic-keynumber-map :transpose 12)
			 :dynamic-map (basic-dynamic-map :scale question-scale))

(general-midi-instrument violin-1 :channel 6 :program 'violin
			 :dynamic-map (basic-dynamic-map :scale violin-1-scale))

(general-midi-instrument violin-2 :channel 7 :program 'violin
			 :dynamic-map (basic-dynamic-map :scale violin-2-scale))

(general-midi-instrument viola :channel 8
			 :dynamic-map (basic-dynamic-map :scale viola-scale))

(general-midi-instrument cello :channel 9
			 :dynamic-map (basic-dynamic-map :scale cello-scale))

(general-midi-instrument ensemble-treble :channel 11 :program 'string-ensemble
			 :dynamic-map (basic-dynamic-map :scale ensemble-high-scale))

(general-midi-instrument ensemble-bass :channel 12 :program 'string-ensemble-2
			 :dynamic-map (basic-dynamic-map :scale ensemble-bass-scale))



(general-midi-metronome :channel 16 :program 'woodblock
			:phrase 72 :bar 72 :beat -1)
