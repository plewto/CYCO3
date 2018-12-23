;;;; CYCO guitar-chords (Oud) Plugin
;;;;
;;;; Defines a guitar chord-model using the Oud Plugin.
;;;; Exports a single object: *GUITAR-CHORD-MODEL* 

(sub-plugin 'oud)
(in-package :cyco-oud)

(param *guitar-chord-model* (make-polychord 'guitar 24 '(e4 a4 d4 g5 b5 e5)))

;; Solo notes
(define-movable-chord *guitar-chord-model* '[solo] 0 (pitch-class 'e) '(0 x x x x x))
(define-movable-chord *guitar-chord-model* '[solo] 0 (pitch-class 'e) '(x x x x x 0))

;; Octaves
(define-movable-chord *guitar-chord-model* '[oct] 0 (pitch-class 'e) '(0 x x x x 0))

;; Power Chords with and without extra octave
(define-movable-chord *guitar-chord-model* '[power] 0 (pitch-class 'e) '(0 2 x x x x)
 :description "Power chord: root & 5th only")
(define-movable-chord *guitar-chord-model* '[power] 0 (pitch-class 'a) '(x 0 2 x x x))
(define-movable-chord *guitar-chord-model* '[power] 0 (pitch-class 'd) '(x x 0 2 x x))
(define-movable-chord *guitar-chord-model* '[power] 0 (pitch-class 'b) '(x x x x 0 2))

(define-movable-chord *guitar-chord-model* '[power2] 0 (pitch-class 'e) '(0 2 2 x x x)
  :description "Power chord 2: root, 5th, octave")
(define-movable-chord *guitar-chord-model* '[power2] 0 (pitch-class 'a) '(x 0 2 2 x x))
(define-movable-chord *guitar-chord-model* '[power2] 0 (pitch-class 'd) '(x x 0 2 3 x))

;; Movable major chords
(define-movable-chord *guitar-chord-model* '[maj] 0 (pitch-class 'e) '(0 2 2 1 0 0)
  :description "Major")
(define-movable-chord *guitar-chord-model* '[maj] 0 (pitch-class 'a) '(x 0 2 2 2 x))
(define-movable-chord *guitar-chord-model* '[maj] 0 (pitch-class 'c) '(x 3 2 0 1 0))
(define-movable-chord *guitar-chord-model* '[maj] 0 (pitch-class 'g) '(x x 0 0 0 3))
(define-movable-chord *guitar-chord-model* '[maj] 0 (pitch-class 'd) '(x x 0 2 3 2))

;; Movable major 'slash' chords with root not in bass position
;;
(define-movable-chord *guitar-chord-model* '[maj-slash] 0 (pitch-class 'd) '(2 x 0 2 3 x)
  :description "'slash' major with root not in bass position")
(define-movable-chord *guitar-chord-model* '[maj-slash] 0 (pitch-class 'c) '(0 x 2 0 1 x))
(define-movable-chord *guitar-chord-model* '[maj-slash] 0 (pitch-class 'g) '(x 2 x 0 3 3))
(define-movable-chord *guitar-chord-model* '[maj-slash] 0 (pitch-class 'f) '(x 0 x   2 1 1))

(define-movable-chord *guitar-chord-model* '[min-slash] 0 (pitch-class 'a) '(0 x 2  2 1 x)
  :description "'slash' minor with root not in bass position")
(define-movable-chord *guitar-chord-model* '[min-slash] 0 (pitch-class 'e) '(x 2 x  0 0 0))
(define-movable-chord *guitar-chord-model* '[min-slash] 0 (pitch-class 'd) '(x 0 3  2 3 x))
(define-movable-chord *guitar-chord-model* '[min-slash] 0 (pitch-class 'd) '(1 x 0  2 3 x))
(define-movable-chord *guitar-chord-model* '[min-slash] 0 (pitch-class 'e) '(3 x 2  0 0 0))
(define-movable-chord *guitar-chord-model* '[min-slash] 0 (pitch-class 'g) '(x 1 x  0 3 3))
(define-movable-chord *guitar-chord-model* '[min-slash] 0 (pitch-class 'a) '(x 3 x  2 1 0))

;; Major 6
(define-movable-chord *guitar-chord-model* '[maj6] 0 (pitch-class 'b) '(x 2 1 1 0 x)
 :description "Major-6  C E G A")
(define-movable-chord *guitar-chord-model* '[maj6] 0 (pitch-class 'f) '(1 x 0 2 1 x))

;; Major 7th
(define-movable-chord *guitar-chord-model* '[maj7] 0 (pitch-class 'a) '(x 0 2 1 2 0)
  :description "Major-7  C E G B")
(define-movable-chord *guitar-chord-model* '[maj7] 0 (pitch-class 'e) '(0 2 1 1 0 0))
(define-movable-chord *guitar-chord-model* '[maj7] 0 (pitch-class 'c) '(x 3 2 0 0 3))
(define-movable-chord *guitar-chord-model* '[maj7] 0 (pitch-class 'f) '(x x 3 2 1 0))
(define-movable-chord *guitar-chord-model* '[maj7] 0 (pitch-class 'd) '(x x 0 2 2 2))
(define-movable-chord *guitar-chord-model* '[maj7] 0 (pitch-class 'e) '(0 x 1 1 0 x))

;; Dominate 7
(define-movable-chord *guitar-chord-model* '[7] 0 (pitch-class 'e) '(0 2 0 1 0 0)
  :description "Dominate-7th  C E G Bb")
(define-movable-chord *guitar-chord-model* '[7] 0 (pitch-class 'b) '(x 2 1 2 0 x))
(define-movable-chord *guitar-chord-model* '[7] 0 (pitch-class 'a) '(x 0 2 0 2 0))

;; 7th sharp-5
(define-movable-chord *guitar-chord-model* '[7+5] 0 (pitch-class 'c) '(x 3 x 3 5 4)
  :description "7th sharp 5  C E G# Bb")
(define-movable-chord *guitar-chord-model* '[7+5] 0 (pitch-class 'c) '(8 x 8 9 9 x))

;; 7th sharp-5 sharp-9
(define-movable-chord *guitar-chord-model* '[7+5+9] 0 (pitch-class 'c) '(x 3 2 3 4 4)
  :description "7th sharp-5 sharp-9   C E G# Bb D#")
(define-movable-chord *guitar-chord-model* '[7+5+9] 0 (pitch-class 'c) '(8 x 8 9 9 11))

;; 7th Sharp-5 Flat-9
(define-movable-chord *guitar-chord-model* '[7+5-9] 0 (pitch-class 'c) '(x 3 2 4 2 4)
 :description "7th sharp-5 flat-9 13th  C E G# Bb Db")
(define-movable-chord *guitar-chord-model* '[7+5-9] 0 (pitch-class 'c) '(8 x 8 9 9 9))

;; 7th sharp-9
(define-movable-chord *guitar-chord-model* '[7+9] 0 (pitch-class 'c) '(x 3 2 3 4 x)
  :description "7th sharp-9  C E G Bb D#")
(define-movable-chord *guitar-chord-model* '[7+9] 0 (pitch-class 'c) '(8 10 8 9 8 11))

;; 7th flat-9
(define-movable-chord *guitar-chord-model* '[7-9] 0 (pitch-class 'c) '(x 3 2 3 2 x)
  :description "7th flat-9  C E G Bb Db")
(define-movable-chord *guitar-chord-model* '[7-9] 0 (pitch-class 'c) '(8 10 8 9 8 9))

;; 7th sus 4
(define-movable-chord *guitar-chord-model* '[7-sus4] 0 (pitch-class 'c) '(x 3 5 3 6 3)
  :description "7th sus 4  C F G Bb")
(define-movable-chord *guitar-chord-model* '[7-sus4] 0 (pitch-class 'c) '(8 10 8 10 8 8))

;; Major 9th
(define-movable-chord *guitar-chord-model* '[maj9] 0 (pitch-class 'c) '(x 2 0  0 1 x)
 :description "Major-9  C E G B D") 
(define-movable-chord *guitar-chord-model* '[maj9] 0 (pitch-class 'f) '(1 0 2 0 x x))
(define-movable-chord *guitar-chord-model* '[maj9] 0 (pitch-class 'f) '(x x 3 0 1 0))

;; Dominate 9
(define-movable-chord *guitar-chord-model* '[9] 0 (pitch-class 'e) '(0 2 0 1 0 0)
 :description "Dominate-9th  C E G Bb D")
(define-movable-chord *guitar-chord-model* '[9] 0 (pitch-class 'as) '(x 1 0 1 1 1))

;; 9th sus 4
(define-movable-chord *guitar-chord-model* '[9-sus4] 0 (pitch-class 'c) '(x 3 3 3 3 3)
 :description "9th sus 4  C F G Bb D")
(define-movable-chord *guitar-chord-model* '[9-sus4] 0 (pitch-class 'c) '(8 10 8 10 8 10))
(define-movable-chord *guitar-chord-model* '[9-sus4] 0 (pitch-class 'c) '(8 x 8 7 6 6))

;; add9
 (define-movable-chord *guitar-chord-model* '[add9] 0 (pitch-class 'c) '(x 3 2 o 3 x)
   :description "Add 9  C E G D")
(define-movable-chord *guitar-chord-model* '[add9] 0 (pitch-class 'c) '(x 3 2 o 3 3))

;; Major 11
(define-movable-chord *guitar-chord-model* '[maj11] 0 (pitch-class 'c) '(x 3 x 4 3 1)
  :description "Major-11th  C E G B D F")

;; Dominate 11
(define-movable-chord *guitar-chord-model* '[11] 0 (pitch-class 'c) '(x 3 x 3 3 1)
  :description "Dominate 11th  C E G Bb D F")

;; Major 13
(define-movable-chord *guitar-chord-model* '[maj13] 0 (pitch-class 'c) '(x 3 x 4 5 5)
  :description "Major 13th  C E G B D F A")

;; Dominate 13
(define-movable-chord *guitar-chord-model* '[13] 0 (pitch-class 'c) '(x 3 x 3 5 5)
  :description "Dominate 13th  C E G Bb D F A")

;; Aug
(define-movable-chord *guitar-chord-model* '[aug] 0 (pitch-class 'c) '(x 3 2 1 1 o)
  :description "Augmented 5  C E G#")

;; Sus 2
(define-movable-chord *guitar-chord-model* '[sus2] 0 (pitch-class 'a) '(x o 2 2 o o)
 :description "Sus 2  C G D")
(define-movable-chord *guitar-chord-model* '[sus2] 0 (pitch-class 'd) '(x x o 2 3 o))
(define-movable-chord *guitar-chord-model* '[sus2] 0 (pitch-class 'e) '(o 2 4 4 o o))

;; Sus 4
(define-movable-chord *guitar-chord-model* '[sus4] 0 (pitch-class 'a) '(x o 2 2 3 o)
 :description "Sus 4  C F G")
(define-movable-chord *guitar-chord-model* '[sus4] 0 (pitch-class 'd) '(x x o 2 3 3))
(define-movable-chord *guitar-chord-model* '[sus4] 0 (pitch-class 'e) '(o 2 2 2 o o))

;; Dim
(define-movable-chord *guitar-chord-model* '[dim] 0 (pitch-class 'c) '(x 3 4 5 4 x)
  :description "Diminished  C Gb C Eb")
(define-movable-chord *guitar-chord-model* '[dim] 0 (pitch-class 'c) '(8 9 10 8 x x))

;; Dim7
(define-movable-chord *guitar-chord-model* '[dim7] 0 (pitch-class 'c) '(x 3 4 2 4 x)
  :description "Diminished  C Gb A Eb")
(define-movable-chord *guitar-chord-model* '[dim7] 0 (pitch-class 'c) '(8 x 7 8 7 x))

;; Min
(define-movable-chord *guitar-chord-model* '[min] 0 (pitch-class 'e) '(0 2 2 0 0 0)
  :description "Minor")
(define-movable-chord *guitar-chord-model* '[min] 0 (pitch-class 'a) '(x 0 2 2 1 0))
(define-movable-chord *guitar-chord-model* '[min] 0 (pitch-class 'd) '(x x 0 2 3 1))

;; Min7
(define-movable-chord *guitar-chord-model* '[min7] 0 (pitch-class 'c) '(8 x 8 8 8 x)
  :description "Minor 7  C Eb G Bb")
(define-movable-chord *guitar-chord-model* '[min7] 0 (pitch-class 'c) '(x 3 5 3 4 3))

;; Min+7
(define-movable-chord *guitar-chord-model* '[min+7] 0 (pitch-class 'c) '(8 x 9 8 8 x)
  :description "Minor with major-7  C Eb G B")
(define-movable-chord *guitar-chord-model* '[min+7] 0 (pitch-class 'c) '(x 3 5 4 4 3))

;; Min7-5
(define-movable-chord *guitar-chord-model* '[min7-5] 0 (pitch-class 'c) '(8 x 8 8 7 x)
  :description "Minor-7 flat-5  C Eb Gb Bb")
(define-movable-chord *guitar-chord-model* '[min7-5] 0 (pitch-class 'c) '(x 3 4 3 4 x))

;; Min add-9 
(define-movable-chord *guitar-chord-model* '[min-add9] 0 (pitch-class 'e) '(o 2 4 o o o)
  :description "Minor add-9   C Eb G D")

(define-movable-chord *guitar-chord-model* '[min6] 0 (pitch-class 'c) '(x 3 1 2 1 3)
  :description "Minor 6th   C Eb G A")

(remove-duplicate-chords *guitar-chord-model*)


(export '(*guitar-chord-model*)
	:cyco-oud)

(import '(cyco-oud:*guitar-chord-model*) :cyco)
(in-package :cyco)
