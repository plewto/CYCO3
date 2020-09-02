;;;; project test-1 preroll
;;;;

(preroll
 :bars 1
 :metronome t
 :instruments (list piano bass organ))

(pass? "alpha preroll created"
       (and (boundp 'preroll)
	    (section-p preroll)))
