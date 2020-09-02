;;;; TEST-3 QBALL
;;;;


(plugin general-midi)
(project test-3 :tempo 60 :bars 4 :beats 4
	 :project-directory (join-path *test-project-directory* "test-3"))

(prune-orchestra)
(general-midi-instrument piano :program 'piano1 :channel 1)
(general-midi-instrument bass :program 'bass-pick :channel 2)
(general-midi-instrument organ :program 'organ1 :channel 3)


(lpf 'alpha)

