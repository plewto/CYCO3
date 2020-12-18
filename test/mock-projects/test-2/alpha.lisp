;;;; CYCO test mock-project test-2 alpha.lisp
;;;;

(section alpha)

(qball qb1 piano
       :bars 2
       :cue '((1 1 1)(1 2 1)(1 3 1))
       :key '(40 41)
       :dur 'w
       :amp 'f)

(let* ((events (render-once qb1))
       (on-events (remove-if-not #'(lambda (q)(midi-note-on-p (cdr q))) events))
       (off-events (remove-if-not #'(lambda (q)(midi-note-off-p (cdr q))) events)))
  (let ((ev1 (car on-events))
	(ev2 (second on-events))
	(ev3 (third on-events)))
    (pass? "QBALL event on-time"
	   (and (zerop (car ev1))
		(= (car ev2)(beat-duration qb1))
		(= (car ev3)(* 2 (beat-duration qb1)))))
    (pass? "QBALL event keys"
	   (and (= (data (cdr ev1) 0) 40)
		(= (data (cdr ev2) 0) 41)
		(= (data (cdr ev3) 0) 40))))
  (let ((ev1 (car off-events))
	(ev2 (second off-events))
	(ev3 (third off-events)))
    (pass? "QBALL event off-times"
	   (and (= (car ev1)(bar-duration qb1))
		(= (car ev2)(+ (bar-duration qb1)(* 1 (beat-duration qb1))))
		(= (car ev3)(+ (bar-duration qb1)(* 2 (beat-duration qb1))))))))
