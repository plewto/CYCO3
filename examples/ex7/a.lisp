;;;; CYCO Examples project ex7 section A
;;;; XBALL Basics.
;;;;

(section a :bars 6)

;; An XBALL is basically a QBALL with chords.   QBALLs have always had the
;; ability to produce chords by using embedded list in the key pattern list,
;; as in   :key  '(c4 e4 g4 (f4 a4 c5)) which produces a c-major arpeggio
;; followed by an f chord.   
;;
(qball a1 piano
       :render-once t
       :cue '((1 1 1)(1 2 1)(1 3 1)(1 4 1))
       :key '(c4 e4 g4 (f4 a4 c5))
       :dur 'q
       :amp 'ff)

;; The XBALL equivalent uses named chords. 
;;
(xball a2 piano
       :render-once t
       :cue '((2 1 1)(2 2 1)(2 3 1)(2 4 1))
       :key '(c4 e4 g4 f4)
       :chord '([solo] [solo] [solo] [maj])
       :dur 'q
       :amp 'ff)

;; Lake most QBALL/XBALL parameters, the chord value may be any arbitrary
;; pattern.  The default pattern is the cycle.  The next example alternates
;; between major and minor chords.
;;
;; Another difference is that a QBALL may take a pattern of
;; instruments while an XBALL may take only a -single- instrument.
;;
(xball a3 piano
       :render-once t
       :cue '((3 1 1)(3 2 1)(3 3 1)(3 4 1))
       :key '(c4 e4 f4 d4)
       :chord '([maj] [min])
       :dur 'q
       :amp 'ff)

;; Use a line pattern to play the first 3-notes as major chords and the
;; reminder as minor
;;
(xball a4 piano
       :render-once t
       :cue '((4 1 1)(4 1 3)(4 2 1) (4 2 3)(4 3 1)(4 3 3)(4 4 1)(4 4 3))
       :key '(c4 f4 g4 d4 e4 a4 e4 d4)
       :chord (line :of '([maj] [maj] [maj] [min]))
       :dur 'e
       :amp 'ff)
       


;; The XBALL borrows heavily from the STRUMMER part.  Most strummer
;; parameters are supported including CHORD-MODEL, INVERSION, OCTAVE, STRUM,
;; DIRECTION and END-TOGETHER.   With exception of CHORD-MODEL and
;; END-TOGETHER, any of these parameters may take a pattern of values.
;;
;; The STRUMMER parameters which are not supported are :STRUM* :AMP*
;; :AMP-BLUR :AMP-LIMITS :CRES :GRACE-AMP* :GRACE-DURATION :GRACE-DELAY
;; :GRACE :BEND :CC :PROGRAM and :BANK.
;;
;; The following example adds an octave note on alternate chords. 
;;
(xball a5 piano
       :render-once t
       :cue '((5 1 1)(5 1 3)(5 2 1) (5 2 3)(5 3 1)(5 3 3)(5 4 1)(5 4 3))
       :key '(c4 d4 e4 f4 g4 a4 b4 c5)
       :chord '([maj] [min] [min] [maj] [maj] [min] [dim] [maj])
       :octave '(0 1)
       :dur 'e
       :amp 'ff)

(->midi a)
