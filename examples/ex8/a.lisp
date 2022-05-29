;;;; ex8 basics of binary cue-list (The hard way)
;;;;

(section a :bars 4 :beats 4 :subbeats 4)

;; The hard way to use a binary-cuelist is to first create a BINCUE
;; object.

(param bc (BINCUE))

;; By default the BINCUE will have the same time-signature as the
;; current-section.  If it is to be used with a part which has a different
;; time-signature, then a matching time-signature must be specified.
;;
;;  (BINCUE :timesig '(2 4 4)) ;; 2 bars, 4 beats per bar, 4 subbeats to the beat.
;;


;; BINCUE provides a cuelist translation between a binary format and the
;; format used by the BAR cue-function.   The following two cuelist produce
;; the same results.
;;
;; BINCUE  '(1000 0010 0010 0000 0000 1000 1000 0000)
;;
;; BAR     '((1 1 0)(1 2 3)(1 3 3)(2 2 1)(2 3 1))
;;
;; There are pros and cons to each format. BINCUE provides a visual
;; indications for exactly which 16th notes are played.  However for long
;; cuelist, short of counting there is no indications where a specific bit
;; occurs.  The BAR format spells out exactly where each event is.   For
;; this reason it is probably best to format each bar of a BINCUE cuelist on
;; a separate line.
;;
;;         '(1000 0010 0010 0000    ;; bar 1
;;           0000 1000 1000 0000)   ;; bar 2
;;


;; Use the BINCUE-TRANSLATE method for the actual cuelist translation.
;;

(qball a-piano-1 piano
       :cue (bincue-translate bc '(1000 0010 0010 0000
				   0000 1000 1000 0000))
       :key '(60)
       :dur 'e
       :amp 'ff)


;; BINCUE also allows symbols for common note patterns.  Use the ?BINCUE
;; method to display a list of available symbols.    
;;
;; (?BINCUE bc)
;; 4-BAR        -> 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000
;; 3-BAR        -> 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000
;; 2-BAR        -> 0000 0000 0000 0000 0000 0000 0000 0000
;; 1-BAR        -> 0000 0000 0000 0000
;; S            -> 1
;; E            -> 10
;; Q            -> 1000
;; H            -> 1000 0000
;; W            -> 1000 0000 0000 0000
;; SR           -> 0
;; ER           -> 00
;; QR           -> 0000
;; HR           -> 0000 0000
;; WR           -> 0000 0000 0000 0000
;; BAR          -> 0000 0000 0000 0000
;;
;; The various N-BAR symbols insert an n-bar rest.   The symbols S E Q H
;; and W insert sixteenth, eighth, quarter, half and whole notes
;; respectively.     The SR ER QR HR and WR symbols inset corresponding rest.
;;
;;
;; The following produces the same events as a-piano-1
;;
;; (bincue-translate bc '(Q ER E ER E QR
;;                        QR Q Q QR)


;; The BINCUE symbol argument allows you to define your own symbols.
;;
;; (param bc (BINCUE :symbol '((clave-1 . "1000 0010 0010 0000")
;;                             (clave-2 . "0000 1000 1000 0000"))))
;; (qball a-piano-2 piano
;;     :cue (bincue-translate '(clave-1 clave-2))
;;     :key '(60)
;;     :dur 'e
;;     :amp 'ff)
