;;;; ex8 BINCUE the easy way.
;;;;

(section b :bars 8 :beats 4 :tempo 96)

;; The BINBALL and BINXBALL macros combine a BINBALL with a QBALL or XBALL
;; parts respectively.   The usage BINBALL and BINXBALL are almost exactly
;; identical to the QBALL and XBALL macros with the following differences.
;;
;;  1) There is no :CUEFN argument, they both use the BAR function.
;;  2) There is a new :USE-SUBBEATS argument.  See section c.
;;  3) There is a new :SYMBOLS argument used to define user BINCUE symbols.
;;  4) The :cue argument takes an optional form for Euclidean rhythms
;;  5) A new :DUCK argument provides ducking the new part in response
;;     to a previous part's events. 
;;
;;  The new objects appear in the project tree as QBALL or XBALL parts.
;;

;; (programs b-programs (list bass guitar)
;; 	  :render-once t)

;; (binball b-kick gm-kick
;; 	 :bars 2
;; 	 :cue '(1010 0000 0010 0100
;; 		1010 0000 0100 0100)
;; 	 :key 'X1
;; 	 :amp 'ff)

;; (binball b-snare gm-snare
;; 	 :bars 2
;; 	 :cue '(0000 1001 0101 1001
;; 		0000 1001 0011 1001)
;; 	 :key '(x1 x2 x2 x2 x1 x2)
;; 	 :amp 'ff)

;; (binball b-hat gm-hihat
;; 	 :bars 2
;; 	 :cue '(1111 1111 1111 1111
;; 		     1111 1111 1111 1111)
;; 	 :duck b-kick
;; 	 :key '(dice (closed closed closed closed open ped))
;; 	 :amp 'ff)

;; (binball b-clave gm-woodblock
;; 	 :bars 2
;; 	 :symbols '((clave-1 . "1000 0010 0010 0000")
;; 		    (clave-2 . "0000 1000 1000 0000"))
;; 	 :cue '(clave-1 clave-2)
;; 	 :key 'clave
;; 	 :amp 'ff)


;; (binball b-bass bass
;; 	 :bars 2
;; 	 :cue '(euclid 0.3 0 nil)
;; 	 :key '(b3 fs5  e4 e4 a3
;; 		   b3 fs5  b4 r e3)
;; 	 :dur '(e. s s s e.)
;; 	 :amp '(mf ff  f f f
;; 		   mf ff   f f f))

;; Define chord-table
;;
(let ((tab (make-hash-table :size 4)))
  (setf (gethash '[min7] tab) '(0 3 10)
	(gethash '[3rd]  tab) '(0 4)
	(gethash '[4th]  tab) '(0 5)
	(gethash '[maj]  tab) '(0 5 9)
	(gethash '[min6] tab) '(0 3 8)
	(gethash '[klstr-1] tab) '(0 4 9 14)
	(gethash '[klstr-2] tab) '(0 5 9 13))
  (param chordtab (make-instance 'chord-table :templates tab)))

	  
;; (binxball b-guitar guitar
;; 	  :chord-model chordtab
;; 	  :bars 2
;; 	  :cue '(1101 0100 1100 1100
;; 		      1101 0100 1111 0000)
;; 	  :key '(b5 b5 fs5   e6   a5 b5   a5 b5
;; 		 b5 b5 a5    b5   c6 b5   a5)
;; 	  :chord '(cycle ([min7] [min7] [3rd] [3rd] [4th] [4th] [4th]
;; 			  [min7] [min7] [min6] [maj] [4th] [4th] [4th]))
;; 	  :direction '(down up)
;; 	  :strum '(dice (0.01 0.01 0.01 0.02 0.03))
;; 	  :end-together t
;; 	  :dur (append (copies 14 's+x) '(q+s))
;; 	  :amp 'mf)


;; (binxball b-scratch guitar
;; 	  :chord-model chordtab
;; 	  :bars 2
;; 	  :cue '(0010 1011 0011 0011
;; 		      0010 1011 0000 0011)
;; 	  :key 'e4
;; 	  :chord '(dice ([klstr-1] [klstr-2]))
;; 	  :direction 'dice
;; 	  :strum '(dice (0.01 0.02))
;; 	  :end-together t
;; 	  :dur 't
;; 	  :amp '(dice (pp pp mp)))

;; (->midi b :filename "loop-b" :repeat 4)


(param spec '(cycle ([min7] [min7] [3rd] [3rd] [4th] [4th] [4th] [min7] [min7] [min6] [maj] [4th] [4th] [4th])))

(param spec '(cycle ([min7] [min7] [3rd] [3rd] [4th] [4th] [4th] [min7] [4th] [4th] [4th])))

;; (param spec '(cycle (A B C D E F G H I J K L M N)))

(param pat (pattern-comprehension spec))

(print pat)
;; (print  (next pat :all))
