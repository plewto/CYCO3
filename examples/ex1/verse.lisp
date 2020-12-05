;;;; CYCO examples ex1 verse.lisp
;;;;



;;; Defines section
(section verse :bars 7)

(metronome verse-metronome)

(simple-part verse-melody flute
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


(simple-part verse-piano-left piano
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

(simple-part verse-piano-right piano
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

(mute verse-metronome   :mute)
(mute verse-melody      nil  )
(mute verse-piano-left  nil  )
(mute verse-piano-right nil  )

(->midi verse :repeat 2)




