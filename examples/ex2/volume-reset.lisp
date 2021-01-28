;;;; CYCO example ex2 section volume-reset
;;;; The final section is used solely to restore instrument volume levels
;;;; after the section F fade-out.

(section volume-reset :bars 4)

(controllers preroll-volume (list piano vibes gm-snare synth guitar)
	     :bars 4
	     :events '((:cc (2 1 1) volume 127)))

(->midi volume-reset)
