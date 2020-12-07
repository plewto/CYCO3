;;;; CYCO example ex1 orchestra.lisp
;;;;
;;;; CYCO does not have an "orchestra" object perse.  Instead the orchestra
;;;; is a linked tree of instruments.   The root of the orchestra tree is
;;;; *ROOT-INSTRUMENT* 
;;;;


;;; Remove old instruments from the orchestra.  If this step were not
;;; taken, every time this file was loaded it would add needless duplicate
;;; instruments to the orchestra.
;;;

(prune-orchestra)

;;; Define the instruments using the general-midi plugin.  We are using a
;;; piano, flute and metronome.
;;;


;;; Create the piano instrument on channel 1.
;;; General-MIDI defines a few different pianos.  In this case the specific
;;; program is 'piano1 which is the "Acoustic Piano".  To see a list of
;;; of available programs enter (?general-midi-programs).
;;;

(general-midi-instrument piano
			 :channel 1
			 :program 'piano1)

;;; Create the flute and vibes instruments on channel 2 and 3.  
;;; There is no need to specify a value for :program since both instrument
;;; names exactly match a general-midi program.

(general-midi-instrument flute
			 :channel 2
			 :keynumber-map (basic-keynumber-map :transpose 12))

(general-midi-instrument vibes
			 :channel 3)


;;; Creates an instrument to use as metronome on channel 16 using the 
;;; general-midi woodblock.  The woodblock is bound to the global
;;; *METRONOME* instrument.
;;;

(general-midi-metronome :channel 16
			:program 'woodblock)


;;; Use the (?o) function to examine the orchestra tree structure.
;;;
