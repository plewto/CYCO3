;;;; CYCO test mock project test-1-main.lisp
;;;;
;;;; QBall
;;;;

(version 3)

(project test-2 :tempo 60 :bars 4 :beats 4
	 :project-directory (join-path *mock-project-directory* "test-2"))

(plugin general-midi)

(general-midi-instrument piano :channel 1 :program 'piano1)

(lpf alpha)
