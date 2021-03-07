;;;; CYCO examples ex2 section e
;;;;
;;;; Part E adds an ersatz guitar.  The synth part drops the lowest 
;;;; octave and is mixed back to give the guitar space.

(param e (clone d :new-name "E"))
(bulk-rename-parts e 1 "E")

;; Remove original synth part, see documentation for nodes.
;; 
(disconnect (get-section-part e 'e-synth))

;; Replacement synth part
;;
(strummer e-synth synth
	     :bars 8
	     :events '(
		       ;; :chord (12 19) drops the low note from the 
		       ;; original part.
		       (:chord (12 19) :amp f :amp* 0.9)
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
		    (:time (8 1 1) :key fs4  :dur h.   )
		    (:time (8 4 1) :key ds4  :dur q    )))


;; Apply 3-cycles triangle wave to pitch-bend in bar 8.
;; 
(bender e-guitar-bend guitar
	:events '((:bend (1 1 1) 0.0)
		  (:time (8 1 1)(8 4 1) t :value 0 -0.50 :cycles 3 :tri)
		  ;; clean up, set pitch bend to 0.
		  ;; The final 16 in the time specification sets the
		  ;; event 16 clock pulses after the start of bar 8, beat 4.
		  (:bend (8 4 4 16) 0.0))) 


;; Add a slight portamento.
;;
(controllers e-guitar-cc guitar
	     :events '((:cc (1 1 1) volume 127)
		       (:cc (1 1 1) portamento-time 8)
		       (:cc (1 1 1) portamento 127)
		       (:cc (8 1 1) portamento 127)))


(->midi e)
(->midi e :filename "loop-e" :repeat 8)
