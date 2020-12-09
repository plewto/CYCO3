;;;; CYCO examples ex1 a.lisp
;;;;
;;;; Defines section A as a more or less literal interpretation
;;;; of the score.


;;; Creates the section object.
;;; 
(section a :bars 7)


;;; Add a metronome.
;;;
(metronome a-metronome)


;;; Use the flute instrument for the melody line.
;;;
;;; The time clauses have the form (bar beat sub-beat)
;;; where sub-beat is the sixteenth note within the beat.
;;; Time specifications are highly flexible, this format 
;;; is simply the default.   For deeper explanation see  
;;; documentation for cue and shuffle functions.  Also see 
;;; the BAR function cheat-sheet in the documentation.
;;;
;;; :dur clause sets note duration.  See metrics and metric-expressions.
;;;
;;; :amp clause sets velocity.  See dynamics.
;;;
;;; The :time, :amp and :dur values remain in effect until
;;; explicitly changed.
;;;

(simple-part a-melody flute
	     :events '((:time (1 1 1) :key d5  :dur e  :amp mf )
		       (:time (1 1 3) :key e5                  )
		       (:time (1 2 1) :key a5                  )
		       (:time (1 2 3) :key e5                  )
		       (:time (2 1 1) :key g5  :dur q          )
		       (:time (2 2 1) :key f5  :dur e          )
		       (:time (2 2 3) :key e5                  )
		       (:time (3 1 1) :key a5  :dur q          )
		       (:time (3 2 1) :key g5                  )
		       (:time (4 1 1) :key d5  :dur h          )
		       (:time (5 1 1) :key f5  :dur q          )
		       (:time (5 1 3) :key a5  :dur e          )
		       (:time (5 2 1) :key c6  :dur e-x        )
		       (:time (5 2 3) :key c6  :dur e          )
		       (:time (6 1 1) :key d6  :dur q          )
		       (:time (6 2 1) :key c6  :dur e          )
		       (:time (6 2 3) :key bf5                 )
		       (:time (7 1 1) :key a5  :dur h.         )))


;;; Add piano left-hand part.
;;;
;;; This part shows two ways to specify a chord.
;;; 1) As a literal list 0f offsets IE (0 12) means play the two notes an octave apart.
;;; 2) [solo] is a symbolic chord name, solo is the equivalent to the offset list (0).
;;; See chord-models
;;; As with :time, :dur and :amp, a :chord value remains in effect until explicitly
;;; changed.
;;;

(simple-part a-piano-left piano
	     :events
	     '((:time (1 1 1) :amp f)
	       (:time (1 1 1) :key d3  :chord (0 12) :dur q )
	       (:time (1 2 1) :key f4  :chord [solo] :dur e )
	       (:time (1 2 3) :key d4                       )
	       (:time (2 1 1) :key e3                :dur q )
	       (:time (2 1 3) :key g4                :dur e )
	       (:time (2 2 1) :key g3                :dur q )
	       (:time (2 2 3) :key bf4               :dur e )
	       (:time (3 1 1) :key a3                :dur q )
	       (:time (3 1 3) :key g4  :chord (0 1)  :dur e )
	       (:time (3 2 1) :key cs4 :chord [solo] :dur q )
	       (:time (3 2 3) :key g4  :chord (0 1)  :dur e )
	       (:time (4 1 1) :key d4  :chord [solo] :dur e )
	       (:time (4 1 3) :key f4                       )
	       (:time (4 2 1) :key e4                       )
	       (:time (4 2 3) :key d4                       )
	       (:time (5 1 1) :key e3                :dur e )
	       (:time (5 1 3) :key f4                       )
	       (:time (5 2 1) :key e4                       )
	       (:time (5 2 3) :key ef4                      )
	       (:time (6 1 1) :key d4                :dur e )
	       (:time (6 1 3) :key bf3               :dur q )
	       (:time (6 2 3) :key e3                :dur e )
	       (:time (7 1 1) :key f3                       )
	       (:time (7 1 3) :key c4                       )
	       (:time (7 2 1) :key g4                       )
	       (:time (7 2 3) :key f4                       )))


;;; Add piano right hand part.
;;; This part has more extensive use of chords.  The symbolic chords
;;; which start with "dy" are two note intervals  IE dy[min3] produces
;;; the root and a minor-third above it.   Use the (?CHORDS) function
;;; to see a list of all symbolic chords.
;;;
;;; The :inv 1 clause at time (1 1 1) produces the first inversion
;;; of the chord.   In this case the minor (0 3 7) chord is played
;;; as (3 7 12).
;;;
;;; The :oct 1 clause at time (7 1 1) adds an octave to the inveted
;;; major chord  (0 4 7) is played as (4 7 12 16).
;;;

(simple-part a-piano-right piano
	     :events
	     '((:time (1 1 1) :dur e :amp f)
	       (:time (1 1 1) :key d4 :chord [min]    :inv 1  )
	       (:time (1 1 3) :key d4 :chord dy[min3] :inv 0  )
	       (:time (1 2 1) :key a5 :chord [solo]           )
	       (:time (1 2 3) :key f5                         )
	       (:time (2 1 1) :key g5                  :dur q )
	       (:time (2 1 3) :key bf4 :chord dy[maj3] :dur e )
	       (:time (2 2 1) :key f5  :chord [solo]          )
	       (:time (2 2 3) :key d5  :chord dy[min2]        )
	       (:time (3 1 1) :key a5  :chord [solo]   :dur q )
	       (:time (3 2 1) :key cs5                 :dur e )
	       (:time (3 2 3) :key g5                  :dur q )
	       (:time (4 1 1) :key d5                  :dur h )
	       (:time (4 1 3) :key a4                  :dur q.)
	       (:time (5 1 1) :key f4 :chord [maj] :inv 2  :dur e)
	       (:time (5 1 3) :key f4              :inv 1        )
	       (:time (5 2 1) :key f5 :chord [maj] :inv 0  :dur e)
	       (:time (5 2 3) :key f5                            )
	       (:time (6 1 1) :key bf4 :chord [maj] :inv 1 :dur q)
	       (:time (6 2 1) :key f5  :chord [maj] :inv 0 :dur e)
	       (:time (6 2 3) :key c4  :chord (0 7 10) :inv 0    )
	       (:time (7 1 1) :key f4  :chord [maj] :inv 1 :oct 1 :dur h.)))

;;; Individual parts may ne muted or soloed

(mute a-metronome   nil )
(mute a-melody      nil  )
(mute a-piano-left  :solo  )
(mute a-piano-right nil  )

(->midi a :repeat 2)
