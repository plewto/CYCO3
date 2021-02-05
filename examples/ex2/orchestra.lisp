;;;; CYCO examples ex2 orchestra.lisp
;;;;


;; Clean orchestra tree.
;;
(prune-orchestra)

;; Define instruments using general-midi.  
;;
(general-midi-instrument piano :channel 1
			 :keynumber-map (basic-keynumber-map :transpose -12))

(general-midi-instrument vibes :channel 2)

(general-midi-instrument synth :channel 3 :program 'fifths ) ;; 'synth-bass)
			 
(general-midi-instrument guitar :channel 4 :program 'eguitar-4)

(general-midi-metronome :channel 16 :program 'woodblock)

;; The general-midi plugin automatically creates several percussion
;; instruments on channel 10.   Use (?o) to view the orchestra structure.
;;


			 
