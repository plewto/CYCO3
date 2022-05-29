;;;; CYCO examples ex7-main
;;;;
;;;;  XBALL parts.
;;;;

(version 3)
(plugin general-midi)
(project ex7 :tempo 90 :bars 4)

(prune-orchestra)
(general-midi-instrument piano :channel 1)
(lpf a)   ;; XBALL Basics.
(lpf b)   ;; Pattern comprehension
