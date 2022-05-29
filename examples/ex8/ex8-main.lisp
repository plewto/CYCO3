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
(general-midi-instrument piano :channel 1)

(lpf a) ;; The basics, the hard way but with important details.
(lpf b) ;; The easy way: BINBALL and BINXBALL.
(lpf c) ;; Complex time signatures.
