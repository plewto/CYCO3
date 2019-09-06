;;;; CYCO Ukulele-chords plugin
;;;;
;;;; Defines Ukulele chord model using Oud plugin.
;;;; Note this model is not 100% accurate:
;;;;    1) Some open chords have been transposed up the neck
;;;;    2) The fret count has been extended.
;;;;
;;;; Exports *UKULELE-CHORD-MODEL*
;;;;

(plugin oud)
(in-package :cyco-oud)

(param *ukulele-chord-model* (make-polychord 'ukulele 16 '(g5 c5 e5 a5)))

(define-movable-chord *ukulele-chord-model* '[solo] 0 (pitch-class 'c) '(x 0 x x))
(define-movable-chord *ukulele-chord-model* '[solo] 0 (pitch-class 'g) '(0 x x x))
(define-movable-chord *ukulele-chord-model* '[maj] 0 (pitch-class 'c) '(0 0 0 3))
(define-movable-chord *ukulele-chord-model* '[7] 0 (pitch-class 'c) '(0 0 0 1))
(define-movable-chord *ukulele-chord-model* '[min] 0 (pitch-class 'c) '(x 3 3 3))
(define-movable-chord *ukulele-chord-model* '[min7] 0 (pitch-class 'c) '(3 3 3 3))
(define-movable-chord *ukulele-chord-model* '[6] 0 (pitch-class 'c) '(0 0 0 0))
(define-movable-chord *ukulele-chord-model* '[maj7] 0 (pitch-class 'c) '(0 0 0 2))
(define-movable-chord *ukulele-chord-model* '[9] 0 (pitch-class 'c) '(0 2 0 1))
(define-movable-chord *ukulele-chord-model* '[dim] 0 (pitch-class 'b) '(0 1 0 1))
(define-movable-chord *ukulele-chord-model* '[aug] 0 (pitch-class 'c) '(1 0 0 3))

;; (dump-chords *ukulele-chord-model*)

(export '(*ukulele-chord-model*) :cyco-oud)
(import '(cyco-oud:*ukulele-chord-model*) :cyco)
(in-package :cyco)
