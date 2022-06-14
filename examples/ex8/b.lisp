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

(binball b-kick gm-kick
	 :bars 2
	 :cue '(1010 0000 0010 0100               ;; BINBALL may not use
		     1010 0000 0100 0100)         ;; pattern-comprehension.
	 :key 'X1
	 :amp 'ff)

(binball b-snare gm-snare
	 :bars 2
	 :cue '(0000 1001 0101 1001
		0000 1001 0011 1001)
	 :key '(x1 x2 x2 x2 x1 x2)
	 :amp 'ff)

(binball b-hat gm-hihat                           ;; Duck the hats on each 
	 :bars 2                                  ;; snare hit.
	 :cue '(1111 1111 1111 1111
		     1111 1111 1111 1111)
	 :duck b-snare
	 :key '(dice (closed closed open open ped))
	 :amp 'ff)

(binball b-conga gm-drum                          ;; The conga uses an inverted
	 :bars 2                                  ;; Euclidean rhythm and is 
	 :cue '(euclid 0.7 2 :invert)             ;; ducked by the kick drum.
	 :duck b-kick
	 :key '(dice (conga-low conga-hi conga-open))
	 :amp 'ff)

(binball b-clave gm-woodblock                    ;; Defines user cue symbols.
	 :bars 2
	 :symbols '((clave-1 . "1000 0010 0010 0000")
		    (clave-2 . "0000 1000 1000 0000"))
	 :cue '(clave-1 clave-2)
	 :key 'clave
	 :amp 'ff)


(binXBall b-bass bass                            ;; Base part defined by binXBALL
	 :bars 4                                 ;; with a Euclidean rhythm
	 :cue '(euclid 0.4 0 nil)
	 :key '(cycle (b3 fs5  e4 e4 a3                    ;; pattern-comprehension
			  b3 fs5  b4 d4 (cycle (e3 d4))))  ;; produces alternate final 
	 :dur '(e. s s s e.  e. s s s s)                   ;; note on each repetition.
	 :amp '(mf ff  f f f
		   mf ff  f f f))

(binXBall b-guitar guitar                                  ;; guitar part uses a spliced
	  :bars 4                                          ;; Euclidean rhythm where bar
	  :cue '(euclid2 3 (0.6 0 nil)(0.4 1 t))           ;; 4 has a different pattern then 
	  :key '(b4 b4 fs5 e4 e4 a4 b4 fs5 d5 c5 b4)       ;; bars 1, 2 & 3.
	  :chord '(dice ([solo] [solo] [solo] [solo] [min]))
	  :strum 0.01
	  :direction '(up down)
	  :dur 's
	  :amp 'mp)

(->midi b :filename "loop-b" :repeat 4)


