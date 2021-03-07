;;;; CYCO examples ex5-main
;;;;
;;;; Example project ex5 illustrates features of the strummer part.
;;;;


(version 3)
(plugin general-midi)

(project ex5 :tempo 90 :bars 4)

(lpf orchestra)
(lpf preroll)
(lpf a)   ;; Basic strummer usage
(lpf b)   ;; Strumming chords
(lpf c)   ;; Alternate chord tables
(lpf d)   ;; Miscellaneous features

;; (project->midi)
