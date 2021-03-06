;;;; CYCO examples ex2 section c
;;;;
;;;; Section C adds "clank" sounds.   Hard to say what sounds were used on
;;;; the original, they almost sound like a shotgun being racked.   Here a
;;;; combination of snare and hand-claps are used.  Dice patterns provide
;;;; random variations. 

(param c (clone b2 :new-name "C"))
(bulk-rename-parts c 2 "C")

;; A Dice pattern selects random snare variations.
;;
(qball c-clanks-1 gm-snare
       :bars 1
       :cue '((1 2 1)(1 3 1))
       :key (dice :of '(x1 x2))
       :amp (cycle :of '(f mf)))

(qball c-clanks-2 gm-snare
       :bars 1
       :cue '((1 2 1)(1 2 3)(1 3 1))
       :key (cycle :of (list (dice :of '(stick stick clap))
			     (dice :of '(clap clap stick))
			     (dice :of '(stick stick clap))))
       :amp (dice :of '(fff ff)))

(->midi c)
       
