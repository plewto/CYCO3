;;;; test-5  strummer
;;;;

(plugin 'general-midi)
(plugin 'ion)
(project test-5 :bars 100 :beats 4 :tempo 60
	 :cuefn #'(lambda (_ time)
		    (dismiss _)
		    (float time)))

(prune-orchestra)

(mu100r piano1 :program 3 :bank 5)

(lpf 'alpha)
(lpf 'grace)
(lpf 'strum)

