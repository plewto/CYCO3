;;;; CYCO Example project ex5 Section A
;;;; Basic Strummer Usage
;;;;

(section A :bars 5)

;; (STRUMMER name instrument &key section cuefn shuffle shift                                
;;                                tempo unit bars beats subbeat
;;                                render-once transposable chord-model
;;                                remarks events)
;;
;; name          - A non-quoted symbol, the parts name.  The new strummer-part
;;                 is bound to the symbol name.  
;; instrument    - Instrument                
;; :section      - Parent section, defaults to the current-section of
;;                 *PROJECT*.              
;; :cuefn        - Event cue function, defaults to section's value.            
;; :shuffle      - Event shuffle function, defaults to section's value.              
;; :shift        - Time shift either in absolute seconds or using relative
;;                 metric value. Defaults to 0.
;; :tempo        - Tempo in BPS, defaults to section's value.          
;; :unit         - Time signature beat-unit, defaults to section's value.           
;; :bars         - Time signature number of bars per phrase, defaults to
;;                 section's value.
;; :beats        - Time signature beats per bar, defaults to section's value.            
;; :subbeats     - Time signature sub-beats per beat, defaults to section's
;;                 value.
;; :render-once  - Boolean, if true do not repeat part.  Render-once has
;;                 no effect if the strummer and part lengths are the same.
;;                 Default nil.
;; :transposable - Boolean, if true the part is subject to transposition.
;;                 Defaults to section's value.
;; :chord-model  - Sets available chord templates, defaults to section's value.                 
;; :remarks      - Optional remarks text.              
;; :events       - List of events to generate.  See below.
;;
;;

;; Strummer events are specified as a nested list of "event clauses".  Each
;; clause starts with a keyword followed by a prescribed number of values.


;; Part A1 plays a c-major arpeggio on bar 1.
;;
(strummer a1 piano
	  :render-once t                     ;; Do not repeat.
	  :events '((:amp ff :dur q)         ;; Sets note velocity and duration until
		                             ;;    explicitly changed.
		    (:time (1 1 1) :key c4)  ;; Generate the notes.
		    (:time (1 2 1) :key e4)  ;; See cue-function docs for 
                    (:time (1 3 1) :key g4)  ;;    for time argument explanation.
		    (:time (1 4 1) :key c5)))

;; Bar 2 is the same as bar 1 but plays each key event as a major chord.
;;
(strummer a2 piano
	  :render-once t
	  :events '((:amp ff :dur q :chord (0 4 7))
		    (:time (2 1 1) :key c4)
		    (:time (2 2 1) :key e4)
		    (:time (2 3 1) :key g4)
		    (:time (2 4 1) :key c5)))

;; Bar 3
;; Chord names may be used.
;; The [solo] chord returns to individual notes.
;; Use the ?CHORDS function to see a list of named chords.
;; 
(strummer a3 piano
	  :render-once t
	   :events '((:amp ff :dur q)
		     (:time (3 1 1) :key c6 :chord [maj])
		     (:time (3 2 1) :key g5             )
		     (:time (3 3 1) :key e5 :chord [min])
		     (:time (3 4 1) :key c5 :chord [solo] :dur h)))


;; Bar 4
;; Chord inversions
;;
(strummer a4 piano
	  :render-once t
	  :events '((:amp ff :chord (0 4 7))
		    ;; original chord  (0 4 7)
		    (:time (4 1 1) :key d4 )

		    ;; first inversion (0 4 7) --> (4 7 12)
		    (:time (4 1 3) :key d4 :inv 1)

		    ;; 2nd inversion (0 4 7) --> (7 12 16)
		    (:time (4 2 1) :key d4 :inv 2)

		    ;; For a 3rd inversion on a 3-note chord,
		    ;; the result is the original transposed 
		    ;; up an octave (0 4 7) --> (12 16 19)
		    (:time (4 2 3) :key d4 :inv 3)  

		    ;; negative inversions are allowed.
		    ;; The original chord becomes (0 4 7) --> (-5 0 4)
		    (:time (4 3 1) :key d4 :inv -1))) 


;; Bar 5
;; Adding octaves to chord
;;
(strummer a5 piano
	  :render-once t
	  :events '((:amp ff :chord (0 4 7))

		    ;; Original chord (0 4 7)
		    (:time (5 1 1) :key e4 )

		    ;; Adds octave copy of first note to end
		    ;; (0 4 7) --> (0 4 7 12)
		    (:time (5 1 3) :key e4 :oct 1)  

		    ;; higher octaves may be added.
		    ;; (0 4 7) --> (0 4 7 24)
		    (:time (5 2 1) :key e4 :oct 2)  

		    ;; If :inv and :oct are used at the same time,
		    ;; the inversion is applied first.
		    ;; (0 4 7) --> (4 7 12) --> (4 7 12 16)
		    (:time (5 3 1) :key e4 :inv 1 :oct 1 :dur h)))


(->midi a)
