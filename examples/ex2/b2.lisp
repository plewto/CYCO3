;;;; CYCO examples ex2 section b2
;;;;
;;;; Adds an alternate vibraphone by inverting and transposing the original.
;;;; The alternate vibes are delayed by a quarter-note.
;;;;

;; Create section B2 as a clone of B1.
;;
(param b2 (clone b1 :new-name "B2"))
(bulk-rename-parts b2 2 "B2")

(qball b2-inverted-vibes vibes
       :bars 8
       :shift 'q  ;; delay by quarter-note
       :cue vibes-cue-list
       :key (transpose (invert vibes-key-list 'f4) 48)  ;; Modify key-list
       :amp 'p+)  ;; reduce amplitude.

(->midi b2)
(->midi b2 :filename "loop-b2" :repeat 16)
