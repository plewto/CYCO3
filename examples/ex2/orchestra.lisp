;;;; CYCO examples ex2 orchestra.lisp
;;;;


(prune-orchestra)

(general-midi-instrument piano
			 :channel 1
			 :program 'piano1
			 :keynumber-map (basic-keynumber-map :transpose -12))
			 

(general-midi-instrument vibes
			 :channel 3)


(general-midi-instrument music-box
			 :channel 3
			 :program 'mbox)




(general-midi-metronome :channel 16
			:program 'woodblock)


			 
