;;;; CYCO example project ex4 Section D
;;;; Dice and Walker patterns


(section d :bars 8 :tempo 130)

;; The DICE pattern returns it's elements at random with replacement.
;; (dice :of '(a b c))
;; (next (dice :of '(A B C)) 9) --> (C B C B C C A B B)
;;
;; This example uses a dice and qball to generate random note events;

(qball d-dice clarinet
       :bars 4 :render-once t
       :cue (create-cue-list :add-eighths t :add-sixteenths t)
       :key (transpose (dice :of (range 0 12)) 60)
       :amp (dice :of '(ppp p mp mf mf mf f ff))
       :dur (dice :of '(s s s e s.)))


;; The WALKER pattern returns a random-walker over it's elements.
;;
(qball d-walker piano
       :bars 4 :render-once t :shift '4*w
       :cue (create-cue-list :add-eighths nil :add-sixteenths t)
       :key (walker :of '(c5 d5 ef5 f5 g5 a5 bf5))
       :amp 'mf
       :dur 'q)

(->midi d)
       

