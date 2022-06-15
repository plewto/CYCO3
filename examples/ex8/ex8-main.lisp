;;;; CYCO examples ex8-main
;;;;
;;;; Binary cue-list
;;;;
;;;; A 'binary cue-list' is an alternate means for expressing event times
;;;; in terms of 16th notes (by default).  They are particularly useful with
;;;; QBALL and XBALL parts.
;;;;

(version 3)
(plugin general-midi)

(project ex8 :bars 4 :beats 4)

(prune-orchestra)
(general-midi-instrument bass :channel 1 :program 33)
(general-midi-instrument guitar :channel 2 :program 30)
(general-midi-instrument piano  :channel 3)


(lpf a) ;; The basics, the hard way but with important details.
(lpf b) ;; The easy way: BINBALL and BINXBALL
        ;; Includes Euclidean rhythms and ducking.
(lpf c) ;; Complex time signatures.
(lpf d) ;; cuelist manipulation with mask-cuelist
