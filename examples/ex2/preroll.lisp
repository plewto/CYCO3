;;;; CYCO examples ex2 preroll.lisp
;;;;

(preroll
 :bars 1
 :instruments (list piano vibes music-box *metronome*))


(->midi preroll)
