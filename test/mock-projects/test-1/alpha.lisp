;;;; project test-1 section alpha


(section alpha :bars 1)
(programs alpha-programs (list piano bass organ))
(metronome alpha-metronome)

(pass? "alpha section created"
       (and (boundp 'alpha)
	    (section-p alpha)))

(pass? "alpha parts created"
       (and (boundp 'alpha-programs)
	    (part-p alpha-programs)
	    (boundp 'alpha-metronome)
	    (part-p alpha-metronome)))
