;;;; CYCO Example ex5 c    Chord Tables
;;;; 


(section c :bars 8)

;; Up to this point all examples have been using the default *CHORD-TABLE*,
;; which takes a keyboardist view.  Notes are generally ordered with no
;; duplicates.  While using *CHORD-TABLE* you may either specify a chord by
;; name or as a template of keynumber offsets.  The following two statements
;; produce identical results. 
;;
;;     (:time (1 1 1) :key 60  :chord [maj]   )
;;     (:time (1 2 1) :key 60  :chord (0 4 7) )
;; 
;; The most important property is that the root keynumber is transposed by
;; each template value.   :key 60 :chord (0 4 7) -->  (60 64 67)
;;

;; Alternate chord tables are available via plugin which take a fretted
;; instrument approach.  These alter both how the root keynumber and the
;; chord template are interpreted.  The most extensive alternate system 
;; is provided by the GUITAR-CHORDS plugin.
;; 

(plugin guitar-chords)

;; Using (?a 'guitar) after loading the plugin, reveals a new bound symbol
;; *GUITAR-CHORD-MODEL*.   Use (?chords *GUITAR-CHORD-MODEL*) to display
;; the chord types it provides.   One thing to notice right away is that
;; guitar-chords defines variations on each generic chord-type.  Further
;; these variations are specific to the root keynumber.  For example there
;; are 9 variations for c-major.
;;

;; (?chords *guitar-chord-model*)

;; While using guitar chords, the keynumber is interpreted differently.
;; Specifically the pitch-class selects a family of chords of the same
;; type and pitch-class, while the octave selects the specific variation.   
;; In general, lower octave numbers are positioned lower on the neck, but 
;; the difference between key c0 and c1 is not strictly an octave.
;;
;;     :key c0 :chord [maj]   --> (R C5 E4 G5 C6 E5)  ;; R indicates a
;;     :key c1 :chord [maj]   --> (R C5 G4 C6 E6 R)   ;; muted string.
;;     :key c2 :chord [maj]   --> (R R G4 C6 E6 C6)
;;
;; c1 is on average higher then c0 but is not an octave above c0.
;;

;; The other major difference is that the chord template is no longer a
;; list of root-note transpositions.  Instead, it is a list of absolute
;; notes to be played.  This also applies if you specify a chord template
;; instead of using a name.
;;
;;  (strummer foo piano      # Using default *chord-table*
;;     :events '((:time (1 1 1) :key 60 :chord (0 4 7))  ;; --> (60 64 67)
;;               (:time (1 2 1) :key 70 :chord (0 4 7))  ;; --> (70 74 77)
;;               ...))
;;
;;  (strummer baz guitar
;;     :chord-model *guitar-chords*
;;     :events '((:time (1 1 1) :key 60 :chord (0 4 7))  ;; --> (0 4 7)
;;               (:time (1 2 1) :key 70 :chord (0 4 7))  ;; --> (0 4 7)
;;               ...))
;;
;; In the 2nd example the actual key number is ignored, it is still
;; required though to produce note events.
;;


(strummer c1 guitar
	  :chord-model *guitar-chord-model*
	  :events '((:chord [min7] :dur e. :amp ff :amp* 0.9 :strum 0.03 :strum* 1.01 :direction down)
		    (:time (1 1 1) :key a0 )
		    (:time (1 1 3) :key a1 )
		    (:time (1 2 1) :key a2 )
		    (:time (1 2 3) :key a3 )
		    (:time (1 3 1) :key a4 )
		    (:time (1 3 3) :key a5 )
		    (:time (1 4 1) :key a6 )
		    (:time (1 4 3) :key a7 )
		    (:time (2 1 1) :key a8 )))
(->midi c)
								       
		    



