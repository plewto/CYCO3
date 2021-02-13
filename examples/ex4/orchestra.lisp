;;;; CYCO examples ex4 orchestra
;;;;

(prune-orchestra)

(general-midi-instrument piano :channel 1)
(general-midi-instrument bass :channel 2)
(general-midi-instrument guitar :channel 3)
(general-midi-metronome :channel 16 :program 'woodblock)


;; gm drum-kit selection
(set-program-number gm-percussion 0)

