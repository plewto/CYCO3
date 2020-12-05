;;;; CYCO example ex1 preroll.lisp
;;;;
;;;; A preroll section is used for initial program channges and as a
;;;; count-in metronome.
;;;;


;;; Create the preroll section of 2 bars length.
;;; Program cahnge events are generated for each instrument in the
;;; instruments list.
;;;
;;; By default a count-in metronome part is alsso created.
;;;
;;; The new section is bound to the symbol PREROLL.
;;;
(preroll :bars 2
	 :instruments (list piano flute *metronome*))

;;; Write the preroll section to a MIDI file.
;;;
(->midi preroll)
