;;;; CYCO examples ex4  c
;;;; Shift register
;;;;

(section c :bars 4 :tempo 120)


;; The shift-register generator is capable of producing a wide array of
;; numeric patterns.  At the extreme these approach pseudo-random.
;;
;;   (shift-register seed taps &key mask hook monitor action prerun)
;;
;; When constructing a shift register it is convenient to use binary notation
;; for the seed, taps and mask values, as this directly represents the
;; state of the register.
;; seed  - initial register value, must be an integer 0 < seed <= 65535.
;;         If seed=0 the register will never produce any other value.
;; taps  - selects which stages are used for feedback.   A 1 indicates
;;         feedback is selected.
;; :mask - Set register length. Typically a register of length n
;;         will have all bits set in an n-length binary word.
;;         Stages may be eliminated from the output by clearing
;;         the corresponding mask bit.  The register has a maximum
;;         length of 16-bits.
;;
;; The taps parameter has the most influence over the generated pattern.
;; Try different combinations.   Usually the left-most taps bit is 1.
;; 

;; It is often the case that a register will produce a wide range of values
;; or very high  values.   The purpose of the hook function is to reign in
;; the natural register values to something more useful.
;;  
;; With a non-zero seed, a register will never produce a zero value
;; The purpose of (1- n) in the hook function is to allow for a zero
;; output.  This example uses the register for keynumbers, the purpose of
;; (rem ... 24) is to coerce the register's output into a 2-octave range.
;; Finally (+ 36 ...) transposes the result up 3 octaves.
;;
(param sr (shift-register #b000001 #b101110 :mask #b111111
			   :hook #'(lambda (n)
				     (+ 36 (rem (1- n) 24)))))

;; (shift-register-period sr) returns the length of the pattern generated
;; by a shift-register.   Note this function is highly inefficient.   The
;; shift-register will be reset after determining it's period.
;;

(param sr-period (pattern-length sr))

;; Display a table of the register pattern.
;; Values on the left in square brackets are just a counter.
;; Numbers to the left  of the arrow are the actual values produced.
;; Numbers to the right of the arrow are the results of applying the hook
;; function and the corresponding key-numbers.
;;
(format t "period ~A~%" sr-period)
(dotimes (i sr-period)
  (let ((cv (current-value sr))
	(val (value sr)))
    (next-1 sr)
    (format t "[~2d] ~3d --> ~3d ~A~%" i cv val (keyname val))))

(reset sr)

;; Create 4-bar cue-list with events on every eighth note.
;;
(let ((cue-list '()))
  (dolist (br '(1 2 3 4))
    (dolist (bt '(1 2 3 4))
      (push (list br bt 1) cue-list)
      (push (list br bt 3) cue-list)))
  (setf cue-list (reverse cue-list))
  
  (qball c-sr-demo piano
	 :cue cue-list
	 :key (next sr sr-period)
	 :dur 'q))

(->midi c :repeat 2)
