;;;; CYCO examples ex2 section e
;;;; Part E adds an ersatz guitar.  The synth part drops the lowest 
;;;; octave and is mixed back to give the guitar space.

(param e (clone d :new-name "E"))

;; Remove the original synth part.  See documentation on nodes.
;;
(disconnect (get-section-part e 'd-synth))

(strummer e-synth synth
	     :bars 8
	     :events '((:chord (12 19) :amp f :amp* 0.9)
		       (:time (1 1 1) :key f5  :dur h.   )
		       (:time (1 4 1) :key f5  :dur q    )
		       (:time (2 1 1) :key fs5 :dur h    )
		       (:time (2 3 1) :key f5  :dur q    )
		       (:time (2 4 1) :key fs5           )
		       (:time (3 1 1) :key f5  :dur h.   )
		       (:time (3 4 1) :key f5  :dur q    )
		       (:time (4 1 1) :key fs5 :dur h    )
		       (:time (4 3 1) :key f5  :dur q    )
		       (:time (4 4 1) :key fs5           )
		       (:time (5 1 1) :key f5  :dur h+e  )
		       (:time (5 3 3) :key f5  :dur e    )
		       (:time (5 4 1) :key fs5           )
		       (:time (5 4 3) :key g5            )
		       (:time (6 1 1) :key gs5 :dur h.   )
		       (:time (6 4 1) :key gs5 :dur e    )
		       (:time (7 1 1) :key ds5 :dur h.   )
		       (:time (7 4 1) :key ds5 :dur q    )
		       (:time (8 1 1) :key fs5 :dur h    )
		       (:time (8 3 1) :key f5  :dur q    )
		       (:time (8 4 1) :key fs5 :dur q    )))

(strummer e-guitar guitar
	  :bars 8
	  :events '((:chord [solo] :amp fff)
		    (:time (1 1 1) :key f4   :dur h+e  )
		    (:time (1 3 3) :key f4   :dur e    )
		    (:time (1 4 1) :key fs4            )
		    (:time (1 4 3) :key g4             )
		    (:time (2 1 1) :key fs4  :dur h+e  )
		    (:time (2 3 3) :key g4   :dur e    )
		    (:time (2 4 1) :key fs4            )
		    (:time (2 4 3) :key ds4            )
		    (:time (3 1 1) :key f4   :dur h+e  )
		    (:time (3 3 3) :key f4   :dur e    )
		    (:time (3 4 1) :key fs4            )
		    (:time (3 4 3) :key g4             )
		    (:time (4 1 1) :key fs4  :dur h+e  )
		    (:time (4 3 3) :key g4   :dur e    )
		    (:time (4 4 1) :key fs4            )
		    (:time (4 4 3) :key ds4            )
		    (:time (5 1 1) :key f4   :dur h+e  )
		    (:time (5 3 3) :key f4   :dur e    )
		    (:time (5 4 1) :key fs4            )
		    (:time (5 4 3) :key g4             )
		    (:time (6 1 1) :key gs4  :dur h.   )
		    (:time (6 4 1) :key gs4  :dur q    )
		    (:time (7 1 1) :key ds4  :dur h.   )
		    (:time (7 4 1) :key ds4  :dur q    )
		    (:time (8 1 1) :key fs4  :dur e    )
		    (:time (8 1 3) :key f4   :dur e    )
		    (:time (8 2 1) :key fs4  :dur e    )
		    (:time (8 2 3) :key f4   :dur e    )
		    (:time (8 3 1) :key fs4  :dur e    )
		    (:time (8 3 3) :key f4   :dur e    )
		    (:time (8 4 1) :key ds4  :dur q    )))

(controllers e-guitar-cc guitar
	     :events '((:cc (1 1 1) portamento-time 64)
		       (:cc (1 1 1) portamento 0)
		       (:cc (8 1 1) portamento 127)))

(->midi e)
(->midi e :filename "loop-e" :repeat 8)
