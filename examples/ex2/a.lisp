;;;; CYCO examples ex2 section a
;;;;
;;;; Section A defines the basic motif using various "qball" parts.
;;;; The percussion instruments are intentionally mixed low. 
;;;;

;; Creates the section object.
;;
(section a :bars 8)


;; Defines the main motif.  The cue-list is shared by the bass, piano 
;; and bass-drum instruments.
;;
(let ((cue-list '((1 1 1)(1 1 3)(1 4 1)(1 4 3)
		  (2 1 1)(2 1 3)(2 4 1)(2 4 3)
		  (3 1 1)(3 1 3)(3 4 1)(3 4 3)
		  (4 1 1)(4 1 3)(4 4 1)(4 4 3)
		  (5 1 1)(5 1 3)(5 3 3)(5 4 1)(5 4 3)
		  (6 1 1)(6 1 3)(6 4 1)(6 4 3)
		  (7 1 1)(7 1 3)(7 4 1)(7 4 3)
		  (8 1 1)(8 1 3)(8 4 1)(8 4 3))))

  ;; The bass part uses a layered piano and bass.
  ;;
  ;; If (list base piano) were replaced with (cycle :of (list base piano)),
  ;; the two instruments would play on alternate notes.
  ;;
  ;; If (dice :of (list bass piano)) were used instead, the bass or piano
  ;; would be randomly selected on each note.
  ;;
  ;; With a qball, patterns may be used for the instrument list, :key,
  ;; :amp and :dur (duration) arguments.  List and, single values, are 
  ;; are converted to cycle patterns.
  ;;
  ;; The :cue argument only takes a list of time-specifications.
  ;;
  (qball a-bass (list bass piano)
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

  
  ;; Define the kick-drum part.
  ;; 
  ;; The General MIDI standard defines two bass drums at keynumbers 35 and
  ;; 36.  The gm-kick instrument uses a keynumber-map to convert the 
  ;; symbols 'X1 and 'X2 to these key-numbers.  The ?KMAP function displays
  ;; information about an instrument's keynumber-map.  To see a list of
  ;; available kick-drum variations, use (?KMAP gm-kick)
  ;;
  (qball a-kick gm-kick
	 :cue cue-list
	 :key '(x1 x2 x1)  ;; cycle kick-drum variation on each note.
	 :reset-on-repeat nil
	 :amp 'f))

(qball a-snare gm-snare
       :bars 1
       :cue '((1 2 1)(1 4 1))
       :key (dice :of '(x1 x1 x1 x2))  ;; select random snare variation.
       :amp 'pp)
       
(qball a-shaker gm-shaker
       :bars 1
       :cue '((1 1 1)(1 1 3)(1 2 1)(1 2 3)
	      (1 3 1)(1 3 3)(1 4 1)(1 4 3))
       :key (dice :of (cons 'maracas (copies 8 'cabasa))) ;; select random shaker variation.
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
  
