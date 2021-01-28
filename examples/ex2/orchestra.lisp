;;;; CYCO examples ex2 orchestra.lisp
;;;;


(prune-orchestra)

(general-midi-instrument piano :channel 1
			 :keynumber-map (basic-keynumber-map :transpose -12))

(general-midi-instrument vibes :channel 2)

(general-midi-instrument synth :channel 3 :program 'synth-bass)
			 
(general-midi-instrument guitar :channel 4 :program 'eguitar-4)

(general-midi-metronome :channel 16 :program 'woodblock)


			 
