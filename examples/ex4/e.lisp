;;;; CYCO example project ex4 Section E
;;;; Coin & wrapper patterns

(section e :bars 8 :tempo 112)


;; The COIN pattern returns a binary choice.  
;;
;;   (COIN &key p head tail length)
;;
;; By default a coin returns either nil or t with 50% probability.    The :p
;; keyword sets the probability of a t result  0 <= p <= 1.
;;  
;; The return value for either head or tail may be a literal value, a
;; pattern or a function of a single argument.   The argument is passed an
;; internal counter.
;;
;; The following example uses a coin to generate qball key-pattern.
;;

(qball e1 piano
       :bars 4 :render-once t
       :cue (create-cue-list)
       :key (coin :head #'(lambda (n)(+ 60 n)) :tail #'(lambda (n)(max 16 (- 60 n))))
       :amp 'f
       :dur 'q)


;; The WRAPPER pattern allow a function to be treated as a pattern.
;;
;;     (WRAPPER &key of period)
;;
;; :of     - The wrapped function of form (lambda n) where n is an
;;           internal counter and there is no prescribed return-type.
;;           The default (lambda (n) n) performs like a cyclical counter. 
;;
;; :period - Integer, maximum value for internal counter, default 16.
;;
;; The following example uses a walker to scan a c-major scale.  The CNTH
;; function is a cyclical version of NTH.  The default counter period of 16
;; causes a periodic glitch in the sequence.
;;
(qball e2 guitar
       :bars 4 :render-once t :shift '4*w
       :cue (create-cue-list :add-sixteenths t)
       :key (wrapper :of #'(lambda (n)(cnth n '(72 74 76 77 79 81 83))))
       :amp 'f
       :dur 'q)


(->midi e)
       
