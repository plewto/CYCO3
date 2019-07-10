;;;; CYCO keytables
;;;;
;;;; A Keytable is an alternate way to map MIDI keynumbers.
;;;; They are simply vectors of length 128.
;;;;
;;;;     tab(q) --> r
;;;;
;;;;   0 <= q < 128
;;;;  -1 <= r < 128
;;;;

(constant +default-keytable+ (->vector (range 0 128)))

(defun keytable-p (obj)
  (and (vectorp obj)
       (= (length obj) 128)
       (every #'(lambda (q)
		  (and (integerp q)
		       (<= +REST+ q)
		       (< q 128)))
	      obj)))


