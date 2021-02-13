;;;; CYCO examples ex4 a
;;;;
;;;; Section a uses QBALLs for a basic drum patteren

(setf *enable-keynumber-warnings* nil)

(section a :bars 4)
(metronome a-metronome)

;; The bass drum part produces 5 events over 2 bars, as specified by the
;; :cue argument.   Since the key-number list has a different length,
;; the generated sounds will cycle relative to the event timing. 
;;
;; gm-kick, the general MIDI bass-drum instrument, produces one of two
;; sounds,  X1 and X2.  These are defined by it's keynumber-map function.
;; Use (?kmap instrument) to see details. 
;; 
;; gm-kick does not recognize 'treat-as-rest as a legitimate key-number.
;; All non-recognized key-numbers are treated as a rest, thus every
;; 4th note is skipped.   This allows two or more instruments to be
;; layered even if they do not recognize the exact same set of
;; key-numbers.   By default there are no warnings when an instrument
;; encounters an unrecognized key-number.  Warnings may be enabled by
;; setting *ENABLE-KEYNUMBER-WARNINGS* to t.
;; 
;; Break down of generated kick-drum events.
;;
;; eighth 
;; note   cue     bar  beat  key 
;;  1     (1 1 1) 1    1     X1
;;  2     (1 1 3)
;;  3     (1 2 1) 1    2
;;  4     (1 2 3)            X1
;;  5     (1 3 1) 1    3
;;  6     (1 3 3)
;;  7     (1 4 1) 1    4     X2
;;  8     (1 4 3)
;;  9     (2 1 1) 2    1
;; 10     (2 1 3)
;; 11     (2 2 1) 2    2     rest
;; 12     (2 2 3)
;; 13     (2 3 1) 2    3
;; 14     (2 3 3)
;; 15     (2 4 1) 2    4     X1         start key repeat
;; 16     (2 4 3)
;;  1     (1 1 1) 3    1     X1         start cue repeat
;;  2     (1 1 3)
;;  3     (1 2 1) 3    2
;;  4     (1 2 3)            X2
;;  5     (1 3 1) 3    3
;;  6     (1 3 3)
;;  7     (1 4 1) 3    4     rest
;;  8     (1 4 3)
;;  9     (2 1 1) 4    1
;; 10     (2 1 3)
;; 11     (2 2 1) 4    2     x1
;; 12     (2 2 3)
;; 13     (2 3 1) 4    3
;; 14     (2 3 3)
;; 15     (2 4 1) 4    4     X1
;; 16     (2 4 3)
;; .....   

(qball a-kick gm-kick
       :bars 2                        ;; Since the section length is
       :cue '((1 1 1)(1 2 3)(1 4 1)   ;; 4-bars, the 2-bar kick part
	      (2 2 1)(2 4 1))         ;; is repeated twice.
       :key '(x1 x1 x2 treat-as-rest)
       :reset-on-repeat t
       :amp 'f)

(qball a-snare gm-snare
       :bars 2
       :cue '((1 2 1)(1 3 3)
	      (2 1 1)(2 3 1)(2 4 3))
       :key '(x1 x1 x2)
       :reset-on-repeat nil  ;; play exact same events on each repetition
       :amp 'f)

(qball a-hats gm-hihat
       :render-once t  ;; Normally this part would repeat every 2 bars
       :bars 2         ;; :render-once causes it to only play the first 2 bars
       :cue '((1 1 1)(1 1 3)(1 2 1)(1 2 3)(1 3 1)(1 3 3)(1 4 1)(1 4 3)
	      (2 1 1)(2 1 3)(2 2 1)(2 2 3)(2 3 1)(2 3 3)(2 4 1)(2 4 3))
       :key (dice :of '(closed closed closed closed open))
       :reset-on-repeat t
       :amp '(p ff))  ;; alternate soft and loud


;; DUMP-EVENTS prints the generated MIDI events.
;; The optional filter takes a single-argument predicate,
;; (lambda (midi-message)) --> Bool.
;; If the filter returns nil the event is printed.
;; This example removes note-off events.
;;
(dump-events a-hats :filter #'midi-note-off-p)

(->midi a)
