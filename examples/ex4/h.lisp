;;;; CYCO3 example ex4 LFO generators
;;;;

(section h :bars 10 :tempo 90)

;; An LFO generator takes a curve as a numeric list, and returns values
;; from the curve in a cycle.   An LFO is superficially like the CYCLE
;; pattern, the following two statements produce the same result:
;;
;;  (next (cycle :of '(1 2 3)) 9)  --> (1 2 3 1 2 3 1 2 3)
;;  (next (lfo :curve '(1 2 3)) 9) --> (1 2 3 1 2 3 1 2 3)
;;
;; An LFO however takes optional monitor, action and hook functions, and is
;; intended to only produce numeric results.
;;

(qball h1 piano
       :bars 2 :render-once t
       :cue (create-cue-list :bars 2 :add-sixteenths t)
       :key (next (lfo :curve '(48 61 73 62 41)) 32)
       :amp 'f)

;; The sawtooth, triangle and pulse functions maybe used for the LFO :curve
;; argument.
;;
(qball h2 guitar
       :bars 2 :render-once t :shift '2*w
       :cue (create-cue-list :bars 2 :add-sixteenths t)
       :key (next (lfo :curve (triangle 48 60 :steps 32)) 32)
       :amp 'ff)

;; There is very little reason to use an LFO in the above example, the
;; triangle function could have directly provided the key-list.   The
;; following example does away with the LFO and ups the triangle
;; cycle-count to 4.
;;
(qball h3 piano
       :bars 2 :render-once t :shift '4*w
       :cue (create-cue-list :bars 2 :add-sixteenths t)
       :key (triangle 48 60 :cycles 4 :steps 32)
       :amp 'ff)

;; An LFO can apply a hook-function to the waveform.
;; Something similar could have been achieved with mapcar.
;;
(qball h3 guitar
       :bars 2 :render-once t :shift '6*w
       :cue (create-cue-list :bars 2 :add-sixteenths t)
       :key (next (lfo :curve (triangle 48 60 :cycles 2 :steps 32)
		       :hook #'(lambda (n)
				 (+ 48 (rem (* n n) 24))))
		  32)
       :amp 'ff)



(->midi h)
