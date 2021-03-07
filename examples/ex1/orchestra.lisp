;;;; CYCO example ex1 orchestra.lisp
;;;;
;;;; CYCO does not have an "orchestra" object per se.  Instead the orchestra
;;;; is a linked tree of instruments.   The root of the orchestra tree is
;;;; *ROOT-INSTRUMENT* 
;;;;


;;; First remove old instruments from the orchestra.  If this step were not
;;; taken, every time this file was loaded it would clog the orchestra
;;; with needless duplication's.
;;;

(prune-orchestra)

;;; Define the instruments using the general-midi plugin.  We are using a
;;; piano, flute, vibes and a metronome.
;;;

;;; Create the piano instrument on channel 1.
;;; Use (?general-midi) for list available general-midi programs.
;;;

(general-midi-instrument piano :channel 1)
			 

;;; Create the flute and vibes instruments on channel 2 and 3.  
;;;

(general-midi-instrument flute :channel 2
			 :keynumber-map (basic-keynumber-map :transpose 12))

(general-midi-instrument vibes :channel 3)


;;; Creates an instrument for use as a metronome using the general-midi woodblock
;;; on channel 16.  The new instrument is bound to the global
;;; *METRONOME* instrument.
;;;

(general-midi-metronome :channel 16
			:program 'woodblock)


;;; Use the (?o) function to examine the orchestra tree structure.
;;;
