;;;; CYCO examples ex1 b.lisp
;;;;

(param b (clone a :new-name "B"))

(bulk-rename-parts b 1 "B")

(param b-metronome (get-section-part b 'b-metronome))
(param b-melody (get-section-part b 'b-melody))
(param b-piano-left (get-section-part b 'b-piano-left))
(param b-piano-right (get-section-part b 'b-piano-right))


(put b-melody :instruments (list vibes))




(mute b-metronome   nil)
(mute b-melody      nil)
(mute b-piano-right nil)
(mute b-piano-left  nil)


(->midi b)
