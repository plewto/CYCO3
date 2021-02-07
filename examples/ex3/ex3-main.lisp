;;;; CYCO examples ex3-main
;;;;


(version 3)
(plugin general-midi)

(project ex3
	 :tempo 90
	 :bars 8
	 :beats 4
	 :title "The Unanswered Question")

(lpf orchestra)
(lpf preroll)
(lpf fin)

(section score :bars 64)
(metronome score-metronome)

(lpf strings)
(lpf question)
(lpf answers)

(section-order '(preroll score reset))

(project->midi)


;; (dump-events (render-project *project*))

