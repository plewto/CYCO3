;;;; CYCO examples ex2 orchestra.lisp
;;;;


;; Clean orchestra tree.
;;
(prune-orchestra)

;; Define instruments using general-midi plugin.
;; Use (?GENERAL-MIDI) for list of available programs.
;;

(general-midi-instrument bass :channel 1)

;; piano-4 (honky tonk) is layered with bass.
;;
(general-midi-instrument piano :channel 2
			 :program 'piano-4
			 :keynumber-map (basic-keynumber-map :transpose -12))


(general-midi-instrument vibes :channel 3)

(general-midi-instrument synth :channel 4 :program 'fifths)
			 
(general-midi-instrument guitar :channel 5 :program 'eguitar-4)

(general-midi-metronome :channel 16 :program 'woodblock)

;; gm drum-kit selection.
;;
(set-program-number gm-percussion 0)

;; The general-midi plugin automatically creates several percussion
;; instruments on channel 10.   Use (?O) to view the orchestra structure.
;;
