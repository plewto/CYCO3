;;;; CYCO examples ex5 f
;;;;
;;;; Logistic generator
;;;; https://en.wikipedia.org/wiki/Logistic_map

;;;; The logistic generator may produces anything from highly repetitive
;;;; to pseudo-random patterns.  The mu value has the most influence over
;;;; the generated sequence and should be in the interval [3.0 4.0).
;;;; mu < ~3.45             --> 2 values
;;;; ~3.45 < mu <= ~3.54    --> 4 values
;;;; mu ~= 3.544            --> 8 values
;;;; mu >= ~3.5669          --> chaos

(section f :bars 4 :tempo 140)

(let* ((cue-list (create-cue-list :bars 4))
       (generator (logistic :seed 0.5 :mu 3.57 :prerun 100
			    :hook #'(lambda (value)
				      (+ 48 (rem (truncate (* 100 value)) 36)))))
       (key-list (next generator (length cue-list))))
  (dump-key-list "Logistic generator" key-list cue-list)

  (qball f-piano piano
	 :cue cue-list
	 :key key-list
	 :dur 'q))

(->midi f)
