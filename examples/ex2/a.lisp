;;;; CYCO examples ex2 section a
;;;;
;;;; Section A defines the basic motif, "a-bass" using a piano, and a few
;;;; simple percussion parts.   The percussion is intentionally mixed low. 
;;;;

;; Creates the section object.
;;
(section a :bars 8)

;; ;; Add MIDI controller events to set all instrument volumes
;; ;; to the maximum.
;; ;; 
;; (controllers preroll-volume (list piano vibes gm-snare synth guitar)
;; 	     :bars 8
;; 	     :events '((:cc (1 1 1) volume 127)))


;; Defines the main motif.  Both the piano and kick-drum share the same
;; cue-list.
;;
(let ((cue-list '((1 1 1)(1 1 3)(1 4 1)(1 4 3)
		  (2 1 1)(2 1 3)(2 4 1)(2 4 3)
		  (3 1 1)(3 1 3)(3 4 1)(3 4 3)
		  (4 1 1)(4 1 3)(4 4 1)(4 4 3)
		  (5 1 1)(5 1 3)(5 3 3)(5 4 1)(5 4 3)
		  (6 1 1)(6 1 3)(6 4 1)(6 4 3)
		  (7 1 1)(7 1 3)(7 4 1)(7 4 3)
		  (8 1 1)(8 1 3)(8 4 1)(8 4 3))))

  (qball a-bass piano
	 :cue cue-list
	 :key '(f3 f3 f3 f3
		   fs3 fs3 fs3 fs3
		   f3 f3 f3 f3
		   fs3 fs3 fs3 fs3
		   f3 f3 f3 fs3 g3
		   gs3 gs3 gs3 gs3
		   ds3 ds3 ds3 ds3
		   fs3 fs3 fs3 fs3)
	 :amp 'fff
	 :dur 'e)

  (qball a-kick gm-kick
	 :cue cue-list
	 :key '(x1 x2 x1)
	 :reset-on-repeat nil
	 :amp 'f ))

(qball a-snare gm-snare
       :bars 1
       :cue '((1 2 1)(1 4 1))
       :key (dice :of '(x1 x1 x1 x2))  ;; select random snare sound for variation.
       :amp 'pp)
       
(qball a-shaker gm-shaker
       :bars 1
       :cue '((1 1 1)(1 1 3)(1 2 1)(1 2 3)
	      (1 3 1)(1 3 3)(1 4 1)(1 4 3))
       :key (dice :of (cons 'maracas (copies 8 'cabasa))) ;; select random shaker sound for variation.
       :amp 'p)

(qball a-clave gm-woodblock
       :bars 1
       :key 'clave
       :cue '((1 2 3)(1 3 3)))


;; Write the section file to the MIDI directory.
;;
(->midi a)

;; Writes an alternate section file which loops 8 times.
;;
(->midi a :filename "loop-a" :repeat 8)
  
