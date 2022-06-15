;;;; CYCO example ex8 C
;;;;
;;;; Use of BINCUE with complex time-signatures.
;;;;
;;;;

;;;; By default BINCUE uses the time-signature subbeat count as the basic
;;;; time unit.  This is usually 4 subbeats to the beat corresponding to 4
;;;; 16th notes per beat.
;;;;
;;;;  '(1000 1000 1000 1000)   ;; Hit on each down beat using 16th notes.
;;;;
;;;; If the :use-subbeat argument is nil, then the tsubbeats are used
;;;; instead.   Each tsubbeat has 2/3 the value of a subbeat.  For the
;;;; default time-signature a tsubbeat corresponds to a 16th note-triplet.
;;;;
;;;; '(100000 100000 100000 100000)  ;; Down beats using tsubbeats.
;;;;

;;;; A time-signatures may have non-standard number of subbeats per beat.
;;;;
;;;;  (section c :bars 1 :beats 4 :subbeats 5)
;;;;
;;;; '(10000 10000 10000 10000)  ;; down beats using 5-subbeats.
;;;;
;;;; When there is an odd number of subbeats, tsubbeats will have a
;;;; fractional value and may not be used with BINCUE.
;;;;

(section c :bars 1 :beats 4 :subbeats 6)

(param bc (bincue :use-subbeats t))
(print (bincue-translate bc '(100000 100000 100000 100000)))


(param bc2 (bincue :use-subbeats nil))
(print (bincue-translate bc2 '(100000000 100000000 100000000 100000000)))

