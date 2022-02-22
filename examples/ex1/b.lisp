;;;; CYCO examples ex1 b.lisp
;;;;
;;;; Defines part B as a variant of A.
;;;; 


;;; Creates section B as clone of A.
;;;
(param b (clone a :new-name "B" :rename-parts t :bind t))

;;; ******************************
;;; *** Prior to version 3.0.1 ***
;;; *****************************
;;; After cloning, all of the b parts still have their original names.
;;; The bulk-rename-parts function replaces the "A" prefix with "B".
;;;
;;; Version 3.0.1 adds :rename-part keyword to the section clone method.
;;; It is no longer necessary to use bulk-rename-parts.
;;;
;; (bulk-rename-parts b 1 "B")


;;; ******************************
;;; *** Prior to version 3.0.1 ***
;;; *****************************
;;; Assigning parts to variables is only necessary if you wish to
;;; manipulate them. 
;;;
;;; Version 3.0.1 adds the :bind keyword to the section clone method.
;;; It is no longer necessary to bind cloned parts manually.
;;;
;; (param b-metronome (get-section-part b 'b-metronome))
;; (param b-melody (get-section-part b 'b-melody))
;; (param b-piano-left (get-section-part b 'b-piano-left))
;; (param b-piano-right (get-section-part b 'b-piano-right))


;;; Replace the original flute instrument with vibes.
;;; 
(put b-melody :instruments vibes)


;;; When cloning a section, groups are not included,  they must be 
;;; manually redefined if desired.
;;;

(group b-piano-group '(b-piano-left b-piano-right))

(mute b-piano-group   nil )
(mute b-metronome   :mute)
(mute b-melody      nil  )
(mute b-piano-left  nil  )
(mute b-piano-right nil  )


(->midi b)
(->midi b :filename "loop-b" :repeat 8)
