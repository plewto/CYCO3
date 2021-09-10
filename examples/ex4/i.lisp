;;;; CYCO examples ex4  i
;;;; Shift register
;;;;

(section i :bars 8 :tempo 120)


;; The shift-register generator is capable of producing a wide array of
;; numeric patterns.  At the extreme, these approach pseudo-random.
;;
;;   (shift-register seed taps &key mask hook monitor action prerun)
;;
;; When constructing a shift register it is convenient to use binary notation
;; for the seed, taps and mask values, binary notation directly represents the
;; register's state.
;;
;; seed  - initial register value, must be an integer 0 < seed <= 65535.
;;         If seed=0 the register will freeze.
;;
;; taps  - selects feedback stages.   
;;         A 1 indicates feedback is selected.
;;
;; :mask - Set register length. Typically a register of length n
;;         will have all bits set in an n-bit binary word.
;;         Stages may be eliminated from the output by clearing
;;         the corresponding mask bit.  The register has a maximum
;;         length of 16-bits.
;;
;; The taps parameter has the most influence over the generated pattern.
;; Try different combinations.   Usually the left-most taps bit is 1.
;;



;; In this example a shift-register generates a sequence of key-numbers.
;; However, shift-registers will often generate values far exceeding 127,
;; the maximum key-number.  The purpose of the hook function is to convert
;; the internal register value to a range useful for key-numbers.
;;
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
  
  (qball i-sr-demo piano
	 :bars 4
	 :cue cue-list
	 :key key-list
	 :dur 'q))

;; Add percussion
;;
(qball i-kick gm-kick
       :bars 2
       :cue '((1 1 1)(1 1 3)
	      (2 1 1))
       :key 'x1
       :amp 'fff)

(qball i-snare gm-snare
       :bars 2
       :cue '((2 1 3)(2 4 3))
       :key 'x1
       :amp '(fff mp))

(qball i-crash gm-cymbal
       :bars 4
       :cue '((4 4 3))
       :key 'crash1
       :amp 'ff)


(metronome i-met)

(->midi i)
