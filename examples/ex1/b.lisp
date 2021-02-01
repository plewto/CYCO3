;;;; CYCO examples ex1 b.lisp
;;;;
;;;; Defines part b as a variant of a.
;;;; 


;;; Creates section B as clone of A.
;;;
(param b (clone a :new-name "B"))

;;; After cloning, all of the b parts still have their original names.
;;; The bulk-rename-parts function replaces the "A" prefix with "B".
;;;
(bulk-rename-parts b 1 "B")

;;; Assigning parts to variables is only necessary if you wish to
;;; manipulate them. 
;;;
(param b-metronome (get-section-part b 'b-metronome))
(param b-melody (get-section-part b 'b-melody))
(param b-piano-left (get-section-part b 'b-piano-left))
(param b-piano-right (get-section-part b 'b-piano-right))


;;; Replace the original flute instrument with vibes.
;;; 
(put b-melody :instruments vibes)


;;; When cloning a section, groups are not included.  They have to
;;; be manually redefined if desired.
;;;

(group b-piano-group (list b-piano-left b-piano-right))


(mute piano-group   nil )

(mute b-metronome   :mute)
(mute b-melody      nil  )
(mute b-piano-left  nil  )
(mute b-piano-right nil  )


(->midi b)
(->midi b :filename "practice-b" :repeat 8)
