;;;; CYCO examples ex2 orchestra.lisp
;;;;


;; Clean orchestra tree.
;;
(prune-orchestra)

;; Define instruments using general-midi.  
;;

(general-midi-instrument bass :channel 2)

(general-midi-instrument piano :channel 2
			 :program 'piano-4 ;; "Honky tonk"
			 :keynumber-map (basic-keynumber-map :transpose -12))


(general-midi-instrument vibes :channel 3)

(general-midi-instrument synth :channel 4 :program 'fifths ) ;; 'synth-bass)
			 
(general-midi-instrument guitar :channel 5 :program 'eguitar-4)

(general-midi-metronome :channel 16 :program 'woodblock)

(set-program-number gm-percussion 127)

;; The general-midi plugin automatically creates several percussion
;; instruments on channel 10.   Use (?o) to view the orchestra structure.
;;


			 
