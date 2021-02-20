;;;; CYCO examples i
;;;;

(section i :bars 8)

(metronome i-metronome)

;; Mainpulating chords
;;
(strummer i-piano piano
	  :bars 8
	  :events '((:amp mf :dur q)
		    ;; Single note  at c4
		    (:time (1 1 1) :key c4  :chord (0)    )
		    
		    ;; c minor, chord stays in effect until changed
		    (:time (1 2 1) :key c4  :chord (0 4 7))  
		    
		    ;; 1-st inversion, chord rendered as (4 7 12)
		    (:time (1 3 1) :key c4 :inv 1         )
		    
		    ;; Negative inversions are possible
		    ;; Original minor-chord rendered as (-5 0 4)
		    (:time (1 4 1) :key c4 :inv -1        )

		    ;; Reset inversion amd add an octave
		    ;; Chord rendered as (0 4 7 12)
		    (:time (2 1 1) :key c4 :inv 0 :oct 1  )

		    ;; Use octave and inversion
		    ;; inversion is applied before the octave inversion
		    ;; Original chord rendered as (4 7 12 16)
		    (:time (2 2 1) :key c4 :inv 1 :oct 1  )
		    
		    ;; Strumming chords
		    ;; The strum argument may be absolute time in seconds
		    ;; or a tempo-synced metric-expression
		    (:chord (0 4 7 12) :inv 0 :oct 0)   ;; reset chord
		    (:time (2 3 1) :key c4 :strum 0.1)  ;; 1/10 second
		    (:time (2 4 1) :key c5 :strum s)    ;; sixteenth note

		    ;; Note order may be changed, options are
		    ;; 'down   - the default, play in order
		    ;; 'up     - reverse
		    ;; 'dice   - select up or down at random
		    ;; 'random - select at random
		    (:time (3 1 1) :key a6 :strum s :direction up)

		    ;; If the direction argument is a list
		    ;; it is treated as a cycle of directions and
		    ;; applied over the next n-chords
		    (:time (3 2 1) :key c5 :strum s :direction (up down random))
		    (:time (3 3 1) :key c6)
		    (:time (3 4 1) :key c7)
		    
		    (:direction down)

		    ;; The rate of strumming may be accelerated.
		    ;; :strum*
		    (:time (4 1 1) :key c4 :chord (0 1 2 3 4 5 6 7 8 9 10 11 12) :strum e :strum* 0.75)

		    ;; or deaccelerated
		    (:time (5 1 1) :key c5 :strum e :strum* 1.1 :direction up)

		    ;; Normally strummed note-off times are staggered
		    ;; If :end-together is true, each note is held until the
		    ;; final note is played
		    (:amp ff :strum s :strum* 1.0 :chord (0 3 7 10 12 ))
		    (:time (6 1 1) :key c4  :direction down :end-together nil :dur s)
		    (:time (7 1 1) :key c6  :direction up   :end-together t   :dur w)
		    ))



(->midi i)
