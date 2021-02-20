;;;; CYCO examples ex4  c
;;;; Shift register
;;;;

(section c :bars 8 :tempo 120)


;; The shift-register generator is capable of producing a wide array of
;; numeric patterns.  At the extreme these approach pseudo-random.
;;
;;   (shift-register seed taps &key mask hook monitor action prerun)
;;
;; When constructing a shift register it is convenient to use binary notation
;; for the seed, taps and mask values, as this directly represents the
;; state of the register.
;;
;; seed  - initial register value, must be an integer 0 < seed <= 65535.
;;         If seed=0 the register will only produce 0.
;;
;; taps  - selects which stages are used for feedback.   A 1 indicates
;;         feedback is selected.
;;
;; :mask - Set register length. Typically a register of length n
;;         will have all bits set in an n-length binary word.
;;         Stages may be eliminated from the output by clearing
;;         the corresponding mask bit.  The register has a maximum
;;         length of 16-bits.
;;
;; The taps parameter has the most influence over the generated pattern.
;; Try different combinations.   Usually the left-most taps bit is 1.
;; 
;; The shift-register is used to generate a sequence of key-numbers.
;; However, they will often generate values far exceeding 127, the maximum
;; key-number.   The purpose of the hook function is to convert the normal
;; register output to a range useful for key-numbers.


(param sr (shift-register #b000001 #b101110 :mask #b111111
			   :hook #'(lambda (n)
				     (+ 36 (rem (1- n) 24)))))

;; The generic function PATTERN-LENGTH attempts to determine the length of a
;; generated pattern before it repeats.
;;
;;    (PATTERN-LENGTH object) --> Integer
;;
;; Usually object will be either a pattern or generator.  Depending on the
;; type object, pattern-length can be highly inefficient.  This is
;; particularly true for random generators (dice, bag, bones, logistic).
;; For this reason pattern-length takes an optional :max keyword.
;;

(let* ((cue-list (create-cue-list))
       (sr-period (pattern-length sr))
       (key-list (next sr sr-period)))

  (dump-key-list "Shift-register key pattern" key-list cue-list)
  
  (qball c-sr-demo piano
	 :bars 4
	 :cue cue-list
	 :key key-list
	 :dur 'q))

;; Add percussion
;;
(qball c-kick gm-kick
       :bars 2
       :cue '((1 1 1)(1 1 3)
	      (2 1 1))
       :key 'x1
       :amp 'fff)

(qball c-snare gm-snare
       :bars 2
       :cue '((2 1 3)(2 4 3))
       :key 'x1
       :amp '(fff mp))

(qball c-crash gm-cymbal
       :bars 4
       :cue '((4 4 3))
       :key 'crash1
       :amp 'ff)


(metronome c-met)

(->midi c)
