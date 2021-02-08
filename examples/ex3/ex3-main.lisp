;;;; CYCO examples ex3-main
;;;;


(version 3)
(plugin general-midi)

(defun tbar (time-signature time-cue)
  (if (eq (car time-cue) 't)
      (let ((bar-number (or (second time-cue) 1))
	    (tripplet-number (or (third time-cue) 1)))
	(+ (* (1- bar-number) (bar-duration time-signature))
	   (* 0.5 (1- tripplet-number) (tbeat-duration time-signature))))
    (bar time-signature time-cue)))


(project ex3
	 :cuefn #'tbar
	 :tempo 60
	 :bars 8
	 :beats 4
	 :title "The Unanswered Question")

(lpf orchestra)
(lpf preroll)
(lpf fin)

(section score :bars 64)
;; (metronome score-metronome)

(lpf strings)
(lpf question)
(lpf answers)

(section-order '(preroll score reset))

(project->midi)


;; (dump-events (render-project *project*))

