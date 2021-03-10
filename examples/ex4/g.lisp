;;;; CYCO Example ex4 G
;;;;
;;;; Counter and Ramp generators.
;;;;

(section g :bars 6 :tempo 90)

;; Generators are like patterns except they only produce numeric values and
;; may not be nested.  They may however appear as an element of a
;; pattern.   The following statement is legal.
;;
;;      (cycle :of (list (counter 0 9)(counter 9 0)))
;;
;; The simplest generator is CONSTANT-VALUE.
;;
;;     (next (constant-value 5) 3) --> (5 5 5)
;;
;; A similar behavior may be produced using a LINE pattern.
;;
;;     (next (line :of '(5)) 3) --> (5 5 5)
;;
;;

;; The next simplest generator is the COUNTER for producing arithmetic
;; sequences. 
;;
;;     (COUNTER from to &key by hook monitor action)
;;
;;     (next (counter 0 9) 15) --> (0 1 2 3 4 5 6 7 8 9 9 9 9 9 9)
;;
;; Superficially the following line statement has identical behavior.
;;
;;     (next (line :of (range 0 10)) 15) --> (0 1 2 3 4 5 6 7 8 9 9 9 9 9 9)
;;
;; However generators may execute an arbitrary function whenever they
;; produce a specific value.   In the following example a counter provides
;; the key-list for a qball.   Initially it produces an increasing
;; circle of 5ths.  When the key exceeds 79 the action function modifies
;; the internal state of the counter to start producing descending
;; minor-3rds. 
;;
;; While this sort of manipulation is possible it can become tricky.  Note
;; that next-n is called on the counter prior to passing it to the qball
;; :key argument.  This is required for the counter to produce a list of
;; values.  Otherwise the internal key-list structure of the qball would
;; have been  (cycle :of <counter-instance>).  It is -not- necessary to call
;; next when passing a pattern to qball.

(qball g1 piano
       :cue (create-cue-list :bars 2 :add-sixteenths t)
       :bars 2 :render-once t
       :key (next-n (counter 24 80 :by 7
			   :monitor #'(lambda (n)
					(>= n 80))
			   :action #'(lambda (cntr v)
				       (setf (initial-value cntr) 74)
				       (setf (final-value cntr) 24)
				       (setf (counter-increment cntr) -3)
				       (reset cntr)
				       74))
		  32)
       :amp 'f)


;; The other major difference between a pattern and a generator is that the
;; later has a value hook-function, while patterns do not.   Hook 
;; functions are applied to the internal-value of the generator prior to
;; the value method returning a result.   Hooks have more relevance in some
;; of the more complex generators.  In the following example the hook
;; converts the nominal chromatic sequence from 30 to 72 into a whole-tone
;; sequence. 

(qball g2 guitar
       :cue (create-cue-list :bars 2 :add-sixteenths t)
       :bars 2 :render-once t :shift '2*w
       :key (counter 30 72 :hook #'(lambda (n)(* n 2)))
       :amp 'ff)

;; Counter generators only produce integer internal-values.   RAMP
;; generators are like counters except they may produce floats.
;;
;; (RAMP from to :key by hook monitor action)
;;


;; The ASR-ENVELOPE generator produces a curve with a attack-sustain-decay
;; countor.

(qball g3 piano
       :cue (create-cue-list :bars 2 :add-sixteenths t)
       :bars 2 :render-once t :shift '4*w
       :key (next-n (asr-envelope 48 72 :attack 4 :sustain 3 :decay 1) 32)
       :amp 'ff)

(->midi g)

