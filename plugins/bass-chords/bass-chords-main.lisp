;;;; CYCO plugin bass-chords
;;;;
;;;; Defines bass guitar chords using Oud plugin
;;;; Exports *bass-chord-model*

(sub-plugin 'oud)
(in-package :cyco-oud)

(param *bass-chord-model* (make-polychord 'bass-guitar-chord-model 24 '(e3 a3 d3 g4)))

;; Solo notes
(define-movable-chord *bass-chord-model* '[solo] 0 (pitch-class 'e) '(0 x x x))
(define-movable-chord *bass-chord-model* '[solo] 0 (pitch-class 'g) '(x x x 0))

;; Power Chords
(define-movable-chord *bass-chord-model* '[power] 0 (pitch-class 'e) '(0 2 x x)
  :description "Power chord  root and 5th only")
(define-movable-chord *bass-chord-model* '[power] 0 (pitch-class 'a) '(x 0 2 x))
(define-movable-chord *bass-chord-model* '[power] 0 (pitch-class 'd) '(x x 0 2))

;; Power chords w added octave
(define-movable-chord *bass-chord-model* '[power2] 0 (pitch-class 'e) '(0 2 2 x)
  :description "Power chords with added octave")
(define-movable-chord *bass-chord-model* '[power2] 0 (pitch-class 'a) '(x 0 2 2))

;; Octaves
(define-movable-chord *bass-chord-model* '[oct] 0 (pitch-class 'e) '(0 x 2 x))
(define-movable-chord *bass-chord-model* '[oct] 0 (pitch-class 'a) '(x 0 x 2)) 

;; Major
(define-movable-chord *bass-chord-model* '[maj] 0 (pitch-class 'ds) '(x 1 1 0)
  :description "Inverted major 5-1-3")

;; 7th
(define-movable-chord *bass-chord-model* '[7] 0 (pitch-class 'e) '(o x o 1))
(define-movable-chord *bass-chord-model* '[7] 0 (pitch-class 'e) '(x 7 6 7))

;; Minor
(define-movable-chord *bass-chord-model* '[min] 0 (pitch-class 'e) '(0 x x 0))
(define-movable-chord *bass-chord-model* '[min] 0 (pitch-class 'e) '(0 2 2 0))
(define-movable-chord *bass-chord-model* '[min] 0 (pitch-class 'e) '(x x 2 0))

;; 4th
(define-movable-chord *bass-chord-model* '[per4] 0 (pitch-class 'e) '(0 0 x x))
(define-movable-chord *bass-chord-model* '[per4] 0 (pitch-class 'd) '(x x 0 0))

;; (remove-duplicate-chords *bass-chord-model*)
;; (dump-chords *bass-chord-model*)

(export '(*bass-chord-model*) :cyco-oud)
(import '(cyco-oud:*bass-chord-model*) :cyco)
(in-package :cyco)
