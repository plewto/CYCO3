;;;; CYCO examples ex3-main
;;;;


(version 3)
(plugin general-midi)

(project ex3
	 :tempo 60
	 :bars 8
	 :beats 4
	 :title "The Unanswered Question")

(lpf orchestra)
(lpf preroll)

(section score :bars 64)
;; (metronome score-metronome)

;; (lpf strings)
(lpf question)



(section-order '(preroll score))

(project->midi)



(dump-events score)
