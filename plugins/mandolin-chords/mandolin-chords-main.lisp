;;;; CYCO Mandolin-chords plugin
;;;;
;;;; Defines Mandolin chord model using Oud plugin.
;;;; Exports *MANDOLIN-CHORD-MODEL*
;;;;

(plugin oud)
(in-package :cyco-oud)

(param *mandolin-chord-model* (make-polychord 'mandolin 20 '(g4 d4 a4 e5)))
(define-movable-chord *mandolin-chord-model* '[solo] 0 (pitch-class 'g) '(0 x x x))
(define-movable-chord *mandolin-chord-model* '[solo] 0 (pitch-class 'e) '(x x x 0))
(define-movable-chord *mandolin-chord-model* '[oct] 0 (pitch-class 'g) '(0 x x 3))
(define-movable-chord *mandolin-chord-model* '[power] 0 (pitch-class 'g) '(0 0 x x))
(define-movable-chord *mandolin-chord-model* '[power] 0 (pitch-class 'd) '(x 0 0 x))
(define-movable-chord *mandolin-chord-model* '[power] 0 (pitch-class 'a) '(x x 0 0))
(define-movable-chord *mandolin-chord-model* '[maj] 0 (pitch-class 'g) '(0 0 2 3))
(define-movable-chord *mandolin-chord-model* '[min] 0 (pitch-class 'g) '(0 0 1 3))

;; Minor 6
(define-movable-chord *mandolin-chord-model* '[min6] 0 (pitch-class 'g) '(0 0 1 0))
(define-movable-chord *mandolin-chord-model* '[min6] 0 (pitch-class 'f) '(1 0 3 1))
(define-movable-chord *mandolin-chord-model* '[min6] 0 (pitch-class 'd) '(2 0 2 1))
(define-movable-chord *mandolin-chord-model* '[min6] 0 (pitch-class 'b) '(1 0 2 2))

;; Dominate 6
(define-movable-chord *mandolin-chord-model* '[6] 0 (pitch-class 'g) '(0 0 2 0))
(define-movable-chord *mandolin-chord-model* '[6] 0 (pitch-class 'f) '(2 0 3 1))
(define-movable-chord *mandolin-chord-model* '[6] 0 (pitch-class 'd) '(2 0 2 2))
(define-movable-chord *mandolin-chord-model* '[6] 0 (pitch-class 'as) '(0 0 1 1))

;; Dominate 6/9
(define-movable-chord *mandolin-chord-model* '[69] 0 (pitch-class 'as) '(3 0 3 3))
(define-movable-chord *mandolin-chord-model* '[69] 0 (pitch-class 'g) '(0 0 0 0))
(define-movable-chord *mandolin-chord-model* '[69] 0 (pitch-class 'c) '(2 0 3 3))
(define-movable-chord *mandolin-chord-model* '[69] 0 (pitch-class 'd) '(2 0 2 0))

;; Dominate 7th flat 5
(define-movable-chord *mandolin-chord-model* '[7-5] 0 (pitch-class 'gs) '(1 0 3 2)
  :description "Dominate 7th flat 5")
(define-movable-chord *mandolin-chord-model* '[7-5] 0 (pitch-class 'c) '(1 0 1 0))

;; Dominate 7th flat 9
(define-movable-chord *mandolin-chord-model* '[7-9] 0 (pitch-class 'gs) '(1 0 3 2)
  :description "Dominate 7th flat 9")
(define-movable-chord *mandolin-chord-model* '[7-9] 0 (pitch-class 'e) '(1 0 2 1))

;; Dominate 7 shrap 5
(define-movable-chord *mandolin-chord-model* '[7+5] 0 (pitch-class 'g) '(0 1 2 1)
   :description "Dominate 7th sharp 5")
(define-movable-chord *mandolin-chord-model* '[7+5] 0 (pitch-class 'e) '(1 0 3 0))
(define-movable-chord *mandolin-chord-model* '[7+5] 0 (pitch-class 'd) '(3 0 3 2))
(define-movable-chord *mandolin-chord-model* '[7+5] 0 (pitch-class 'as) '(1 0 1 2))

;; Dominate 7
(define-movable-chord *mandolin-chord-model* '[7] 0 (pitch-class 'g) '(0 0 2 1))
(define-movable-chord *mandolin-chord-model* '[7] 0 (pitch-class 'g) '(0 3 2 3))
(define-movable-chord *mandolin-chord-model* '[7] 0 (pitch-class 'd) '(x 0 3 2))
(define-movable-chord *mandolin-chord-model* '[7] 0 (pitch-class 'e) '(1 0 2 0))
(define-movable-chord *mandolin-chord-model* '[7] 0 (pitch-class 'as) '(1 0 1 1))

;; Major 7
(define-movable-chord *mandolin-chord-model* '[maj7] 0 (pitch-class 'g) '(0 0 2 2))
(define-movable-chord *mandolin-chord-model* '[maj7] 0 (pitch-class 'd) '(2 0 4 2))
(define-movable-chord *mandolin-chord-model* '[maj7] 0 (pitch-class 'as) '(3 0 0 1))
(define-movable-chord *mandolin-chord-model* '[maj7] 0 (pitch-class 'as) '(3 0 0 x))

;; Minor 7
(define-movable-chord *mandolin-chord-model* '[min7] 0 (pitch-class 'g) '(0 0 1 1))
(define-movable-chord *mandolin-chord-model* '[min7] 0 (pitch-class 'e) '(0 0 2 0))
(define-movable-chord *mandolin-chord-model* '[min7] 0 (pitch-class 'd) '(2 0 3 1))
(define-movable-chord *mandolin-chord-model* '[min7] 0 (pitch-class 'b) '(2 0 2 2))

;; Minor sharp 7
(define-movable-chord *mandolin-chord-model* '[min+7] 0 (pitch-class 'g) '(0 0 1 2))
(define-movable-chord *mandolin-chord-model* '[min+7] 0 (pitch-class 'e) '(0 1 2 0))
(define-movable-chord *mandolin-chord-model* '[min+7] 0 (pitch-class 'b) '(3 0 2 2))

;; Minor 7 flat 5
(define-movable-chord *mandolin-chord-model* '[min7-5] 0 (pitch-class 'g) '(0 0 1 1)
   :description "Minor 7 flat 5")
(define-movable-chord *mandolin-chord-model* '[min7-5] 0 (pitch-class 'e) '(0 0 2 0))
(define-movable-chord *mandolin-chord-model* '[min7-5] 0 (pitch-class 'd) '(2 0 3 1))
(define-movable-chord *mandolin-chord-model* '[min7-5] 0 (pitch-class 'b) '(2 0 2 2))

;; Dominate 9
(define-movable-chord *mandolin-chord-model* '[9] 0 (pitch-class 'g) '(0 0 0 1))
(define-movable-chord *mandolin-chord-model* '[9] 0 (pitch-class 'e) '(1 0 2 2))
(define-movable-chord *mandolin-chord-model* '[9] 0 (pitch-class 'c) '(0 0 1 0))
(define-movable-chord *mandolin-chord-model* '[9] 0 (pitch-class 'as) '(1 0 3 1))
(define-movable-chord *mandolin-chord-model* '[9] 0 (pitch-class 'g) '(2 0 2 1))

;; Major 9
(define-movable-chord *mandolin-chord-model* '[maj9] 0 (pitch-class 'ds) '(0 0 1 1))
(define-movable-chord *mandolin-chord-model* '[maj9] 0 (pitch-class 'c) '(0 0 2 0))
(define-movable-chord *mandolin-chord-model* '[maj9] 0 (pitch-class 'as) '(2 0 3 1))
(define-movable-chord *mandolin-chord-model* '[maj9] 0 (pitch-class 'g) '(2 0 2 2))

;; Minor 9
(define-movable-chord *mandolin-chord-model* '[min9] 0 (pitch-class 'e) '(0 0 2 2))
(define-movable-chord *mandolin-chord-model* '[min9] 0 (pitch-class 'b) '(2 0 4 2))

;; Diminished
(define-movable-chord *mandolin-chord-model* '[dim] 0 (pitch-class 'gs) '(1 0 2 1))
(define-movable-chord *mandolin-chord-model* '[dim] 0 (pitch-class 'd) '(1 0 2 1))

;; Augmented
(define-movable-chord *mandolin-chord-model* '[dim] 0 (pitch-class 'g) '(0 1 2 3))

;; Suspended
(define-movable-chord *mandolin-chord-model* '[sus4] 0 (pitch-class 'g) '(0 0 3 3))


;; (dump-chords *mandolin-chord-model*)

(export '(*mandolin-chord-model*) :cyco-oud)
(import '(cyco-oud:*mandolin-chord-model*) :cyco)
(in-package :cyco)

