;;;; CYCO examples ex4 A
;;;;
;;;; Section A uses QBALLs to create a basic drum pattern.
;;;; Qballs are heavily reliant on implicit CYCLE patterns.
;;;;
;;;; The CYCLE pattern returns its element in sequence.  Once the final
;;;; value has been reached, the cycle repeats.
;;;;
;;;; The :KEY, :DUR and :AMP arguments to QBALL may take any of the
;;;; following types:
;;;;
;;;;   1) A single appropriate value (keynumber, metric-expression or
;;;;      dynamic respectively).
;;;;   2) A list of values.
;;;;   3) A pattern of values.
;;;;
;;;; If any of these values are a single atom or a list, it is converted
;;;; to a CYCLE pattern.   If the value is some type of PATTERN, then that
;;;; pattern is used directly.

(setf *enable-keynumber-warnings* nil)

(section a :bars 4)
(metronome a-metronome)

;; The bass drum part produces 5 events over 2 bars as specified by the
;; :cue argument.   Since the key-number list has a different length,
;; the generated sounds will cycle relative to the event timing. 
;;
;; gm-kick, the general MIDI bass-drum instrument, produces one of two
;; sounds:  X1 and X2.  These are defined by it's keynumber-map function.
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


(qball a-kick gm-kick
       :bars 2                        ;; Since the section length is
       :cue '((1 1 1)(1 2 3)(1 4 1)   ;;   4-bars, the 2-bar kick part
	      (2 2 1)(2 4 1))         ;;   is repeated twice.
       :key '(x1 x1 x2 treat-as-rest) ;;
       :reset-on-repeat t             ;; Each repetition is identical.
       :amp 'f)

(qball a-snare gm-snare              ;; By setting :reset-on-repeat nil,
       :bars 2                       ;;   the key-number pattern is not
       :cue '((1 2 1)(1 3 3)         ;;   reset when the snare pattern repeats.
	      (2 1 1)(2 3 1)(2 4 3)) ;;   The second time through the 
       :key '(x1 x1 x2)              ;;   key-pattern will be shifted 
       :reset-on-repeat nil          ;;   relative to the first iteration.
       :amp 'f)                      ;;   
                         
 

(qball a-hats gm-hihat
       :render-once t  ;; Normally this part would repeat every 2 bars,
       :bars 2         ;; :render-once causes it to only play the first 2 bars.
       :cue '((1 1 1)(1 1 3)(1 2 1)(1 2 3)(1 3 1)(1 3 3)(1 4 1)(1 4 3)
	      (2 1 1)(2 1 3)(2 2 1)(2 2 3)(2 3 1)(2 3 3)(2 4 1)(2 4 3))
       :key (dice :of '(closed closed closed closed open))
       :reset-on-repeat t
       :amp '(p ff))  ;; alternate soft and loud


;; DUMP-EVENTS prints the generated MIDI events.
;;    The optional filter takes a single-argument predicate,
;;    (lambda (midi-message)) --> Boolean.
;;    If the filter returns nil the event is printed.
;;    This example suppresses note-off events.
;;
(dump-events a-hats :filter #'midi-note-off-p)

(->midi a)
(->midi a :filename "loop-a" :repeat 8)
