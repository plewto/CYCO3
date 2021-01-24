;;;; test 12 channel-assignments
;;;;

(set-meta-channel 'alpha 1)
(set-meta-channel 'beta 2)
(set-meta-channel 'drums 10 "Drum machine")
(set-meta-channel 'snare 'drums)
(set-meta-channel 'ape 1)
(set-meta-channel 'apple 1)
(set-meta-channel 'rim-shot 'snare)



(pass? "meta-channel test 1"
       (and (= (meta-channel 3) 3)
	    (= (meta-channel 'alpha) 1)
	    (= (meta-channel 'beta) 2)
	    (= (meta-channel 'drums) 10)
	    (eq (meta-channel 'snare nil) 'drums)
	    (eq (meta-channel 'rim-shot nil) 'snare)
	    (= (meta-channel 'rim-shot t) 10)))

(pass? "meta-channel test 2"
       (and (every #'meta-channel-assignment-p (range 1 17))
	    (every #'meta-channel-assignment-p '(alpha beta drums snare))
	    (not (meta-channel-assignment-p 17))
	    (not (meta-channel-assignment-p 'dog))))
