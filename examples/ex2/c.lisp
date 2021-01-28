;;;; CYCO examples ex2 section c
;;;; Section C adds "clank" sounds.   Hard to say what sounds were used on
;;;; the original, they almost sound like a shotgun being racked.   Here a
;;;; combination of snare and hand-claps are used.  Dice patterns
;;;; are used for random variations. 

(param c (clone b :new-name "C"))

(qball c-clanks-1 gm-snare
       :bars 1
       :cue '((1 2 1)(1 3 1))
       :key (dice :of '(x1 x2))  ;; x1 and x2 are two snare sound variations
       :amp (cycle :of '(f mf))) ;; defined by the general-midi plugin.

(qball c-clanks-2 gm-snare
       :bars 1
       :cue '((1 2 1)(1 2 3)(1 3 1))
       :key (cycle :of (list (dice :of '(stick stick clap))
			     (dice :of '(clap clap stick))
			     (dice :of '(stick stick clap))))

       :amp (dice :of '(fff ff)))

(->midi c)
       
