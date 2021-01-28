;;;; CYCO examples ex2 section d
;;;;
;;;; Section D adds a synth playing parallel 5ths. 
;;;; Your general-midi mileage may vary.
;;;;

(param d (clone c :new-name "D"))
(bulk-rename-parts d 1 "D")

(strummer d-synth synth
	     :bars 8
	     :events '((:chord (0 7) :amp f :amp* 0.9) ;; Defines the parallel fifths used throughout.
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
(->midi d)
(->midi d :filename "loop-d" :repeat 8)
