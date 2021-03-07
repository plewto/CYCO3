;;;; CYCO Example ex5 d   Miscellaneous strummer features.
;;;; 

(section d :bars 12)

;; Dynamics.
;;
(strummer d1 piano
	  :render-once t
	  :events '((:chord [solo] :dur q)
		    ;; Amplitude may be a single value or a list of values
		    ;; applied over the next n events.
		    (:amp mf)
		    (:time (1 1 1) :key c5)  ;; mf
		    (:time (1 1 3) :key e5)  ;; mf
		    (:time (1 2 1) :key g5)  ;; mf
		    (:time (1 2 3) :key c6 :dur h) ;; mf

		    ;; If a list is used, it is applied in a cyclical manner over
		    ;; the next n-events
		    (:amp (p mf f))
		    (:time (2 1 1) :key d5 :dur q) ;; p
		    (:time (2 1 3) :key f5 )       ;; mf
		    (:time (2 2 1) :key a5 )       ;; f
		    (:time (2 2 3) :key d6 :dur h) ;; p

		    ;; :cres start end count
		    ;; Sets up crescendo or decrescendo over the next count
		    ;; notes.
		    (:cres pp fff 8)
		    (:time (3 1 1) :key f5  :dur e) ;; pp
		    (:time (3 1 2) :key g5  )
		    (:time (3 1 3) :key a5  )
		    (:time (3 1 4) :key bf5 )
		    (:time (3 2 1) :key c6  )
		    (:time (3 2 2) :key d6  )
		    (:time (3 2 3) :key e6  )
		    (:time (3 2 4) :key f6  ) ;; fff

		    ;; The dynamic level remains at final level of the crescendo
		    (:time (3 3 1) :key f6  ) ;; fff
		    (:time (3 3 2) :key f6  ) ;; fff
		    (:time (3 3 3) :key f6  ) ;; fff
		    (:time (3 3 4) :key f6 :dur q)

		    ;; Amplitude levels may be randomized with   :amp-blur n
		    ;; where 0 <= n <= 1
		    ;; For n == 1, amplitude is completely random.
		    ;; As n approaches 0, the resulting amplitude has higher
		    ;; probability of being the specified nominal amplitude.
		    (:time (4 1 1) :key g6 :dur q :amp ff :amp-blur 0.5)
		    (:time (4 1 2) :key g6 )
		    (:time (4 1 3) :key g6 )
		    (:time (4 1 4) :key g6 )
		    (:time (4 2 1) :key g6 )
		    (:time (4 2 2) :key g6 )
		    (:time (4 2 3) :key g6 )
		    (:time (4 2 4) :key g6 )

		    ;; Finally the amplitude may be restricted to a specified
		    ;; range.  Values outside of the range are clipped.
		    ;; :amp-limits low high
		    (:time (5 1 1) :key a6 :dur q :amp ff :amp-blur 1.0 :amp-limits mp mf)
		    (:time (5 1 2) :key a6 )
		    (:time (5 1 3) :key a6 )
		    (:time (5 1 4) :key a6 )
		    (:time (5 2 1) :key a6 )
		    (:time (5 2 2) :key a6 )
		    (:time (5 2 3) :key a6 )
		    (:time (5 2 4) :key a6 )
		    
		    ;; The following keywords define how grace-notes are played.
		    ;; :grace-amp* scale      0.1 <= scale <= 4.0
		    ;;      Sets grace-note amplitude relative to nominal amplitude.
		    ;;
		    ;; :grace-duration m
		    ;;      Sets grace-note duration in either absolute seconds or
		    ;;      as a metric expression.
		    ;;
		    ;; :grace-delay time
		    ;;      Sets the grace-note on-time relative to the nominal note
		    ;;      time.   Time may either be in absolute seconds or a
		    ;;      metric-expression.  Negative values move the grace-note
		    ;;      before the nominal time, positive value place is after.
		    ;;      
		    ;; :grace key
		    ;;      Keynumber for grace note.   Chords are not applied to
		    ;;      grace notes.
		    ;;
		    (:grace-amp* 0.9 :grace-duration t :grace-delay -0.2)
		    (:time (6 1 1) :key c6 :chord [min7] :dur w :amp fff)
		    (:grace gs7)

		    ;; Transmits program-change events.
		    ;; :program n
		    ;; The resulting events are dependent on instrument's program-number-map
		    ;; Try (?pmap instrument) for details.  In the case of the general-midi
		    ;; piano the program-map is simple and only recognizes numeric
		    ;; program numbers.   :program 16, switches to an organ sound.
		    ;;
		    (:time (7 1 1) :program 16)
		    (:time (7 2 1) :key a5 :chord [min7] :amp mf :dur h.)

		    ;; :program default  is a special case to switch to the instrument's
		    ;; default program.
		    ;;
		    (:time (7 4 4) :program default)
		    

		    ;; Generate single controller events
		    ;; :cc controller-number value
		    ;; 0 <= controller-number < 128
		    ;; 0 <= value < 127 
		    ;; Use CONTROLLERS part for more feature-rich controller and
		    ;; after-touch events.   cc 7 is volume.
		    ;;
		    (:time (8 1 1) :cc 7  64 :key a4 :chord [circle-4] :amp ffff :dur w+w)
		    (:time (8 2 3) :cc 7 127)
		    
		    ;; :bend n   produces a single bend event.
		    ;; -1.0 <= n <= +1.0,   0 --> no bend.
		    ;; Use BENDER part for more extensive pitch-bend control.
		    ;;
		    (:time (9 1 1) :key a5 :chord [maj9] :amp ffff :dur w+w)
		    (:time (9 1 2) :bend -1.0)
		    (:time (9 1 3) :bend  1.0)
		    (:time (9 1 4) :bend -0.667)
		    (:time (9 2 1) :bend  0.667)
		    (:time (9 2 2) :bend -0.333)
		    (:time (9 2 3) :bend  0.333)
		    (:time (9 2 4) :bend  0.000)))

(->midi d)
								       
		    



