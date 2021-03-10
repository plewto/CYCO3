;;;; CYCO examples ex4
;;;; Nested patterns.
;;;;

(section f :bars 4 :tempo 120)

;; Patterns may be nested to any depth.
;; The following example nest pattern 3-deep.
;;   The first time through the initial sequence (c5 d5 ef5 f5 gf5) is played.
;;   Thereafter the first cycle repeatadly plays (a5 c6 ef6) with the final
;;   cycle selecting alternate ending notes (gf6 a6) before repeating.
;;
(qball f1 piano
       :cue (create-cue-list)
       :key (line :of (append '(c5 d5 ef5 f5 gf5)
			      (list (cycle :of (append '(a5 c6 ef6)
						       (list (cycle :of '(gf6 a6))))))))
       :dur 'q)

(->midi f)
