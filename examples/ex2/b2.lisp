;;;; CYCO examples ex2 section b2
;;;; Adds a counter-vibraphone by inverting and transposing the original
;;;; The counter-vibes are delayed by a quarter-note.

;; Create section B2 as a clone of B1.
;;

(param b2 (clone b1 :new-name "B2"))

(bulk-rename-parts b2 1 "B2")

(qball b2-inverted-vibes vibes
       :bars 8
       :shift 'q  ;; delay by quarter-note
       :cue vibes-cue-list
       :key (transpose (invert vibes-key-list 'f4) 48)  ;; Modify key-list
       :amp 'p+)

(controllers b2-vibes-cc vibes
	     :bars 8
	     :events '((:cc (1 1 1) portamento-time 4)
		       (:cc (1 1 1) portamento 127)))

(->midi b2)
(->midi b2 :filename "loop-b2" :repeat 16)
