;;;; CYCO generators/euclid.lisp
;;;;
;;;; Generates Euclid pattern.
;;;; https://en.wikipedia.org/wiki/Euclidean_rhythm
;;;; https://dbkaplun.github.io/euclidean-rhythm/
;;;;
;;;; TODO: Update documentation.

(in-package :cyco)

(defclass euclid (generator)
  ((length
    :type integer
    :initform 8
    :initarg :length)
   (points
    ;; normally 0 < points <= length.
    ;; When points > length, duplicate values appear in the results and
    ;; pattern-length method returns spurious value.
    :type integer
    :initform 4
    :initarg :points)
   (increment
    :type number
    :initform 2
    :initarg :increment)
   (shift
    :type integer
    :initform 0
    :initarg :shift))
  (:documentation
   "EUCLID is a generator for producing 'Euclidean Rhythms'
https://en.wikipedia.org/wiki/Euclidean_rhythm"))


(defmethod reset ((e euclid))
  (setf (internal-value e) 0)
  e)

(defun euclid (length points &key (shift 0)
		      (monitor #'(lambda (value)
				   (declare (ignore value))))
		      (action #'(lambda (value) value))
		      (hook #'(lambda (n) n)))
  (if (not (and (numberp length)(plusp length)))
      (cyco-error (sformat "Euclid length argument must be positive number, got ~A" length)))
  (if (not (and (numberp points)))
      (cyco-error (sformat "Euclid points argument must be positive number, got ~A" points)))
  (reset (make-instance 'euclid
			:length length
			:points points
			:increment (/ length points)
			:shift shift
			:monitor monitor
			:action action
			:hook hook)))

(defmethod clone ((mother euclid) &key &allow-other-keys)
  (euclid (slot-value mother 'length)
	  (slot-value mother 'points)
	  :shift (slot-value mother 'shift)
	  :hook (value-hook mother)
	  :monitor (monitor mother)
	  :action (action mother)))

(defmethod value ((gen euclid))
  (funcall (slot-value gen 'value-hook)
	   (mod (+ (slot-value gen 'shift)
		   (round (slot-value gen 'internal-value)))
		(slot-value gen 'length))))

(defmethod next-1 ((gen euclid))
  (prog1
      (progn
	(if (funcall (monitor gen)(internal-value gen))
	    (funcall (action gen) gen))
	(value gen))
    (setf (internal-value gen)
	  (+ (internal-value gen)
	     (slot-value gen 'increment)))))
	  

;; NOTES: Returns incorrect value when points > length.
;;
(defmethod pattern-length ((eclid euclid) &key &allow-other-keys)
  (let* ((guard 5000)
	 (seen '())
	 (points 0)
	 (gen (clone eclid))
	 (value 0))
    (while (not (and (member value seen)(< points guard)))
      (push value seen)
      (setf value (next-1 gen))
      (setf points (1+ points)))
    (1- points)))


(setf (documentation 'euclid 'function)
      "Creates 'Euclidean Rhythm' Generator.

https://en.wikipedia.org/wiki/Euclidean_rhythm
https://dbkaplun.github.io/euclidean-rhythm/

The sequence is produced by dividing length as evenly as possible by
points, where both length and points are positive integers.  Typically 
0 < length, and 0 < points <= length.  If length and points have a GCD > 1,
then the generated values are evenly spaced.   If length and points are
relatively prime, the generated values are not evenly spaced.   It is this
later case which produces the most interesting results.  

Although typically points <= length, this implementation allows 
points > length.  For points > length, duplicate values may appear. 

The most common application is for rhythmic generation.  For example, for a
length of 8, there are 8 possible resulting values: 0, 1, 2, ..., 7.  These
may be treated as all of the 8th notes of a bar in 4/4.  The points
argument determines which of the possible 8th notes are to be played. 

    (param foo (euclid 8 4))
    (next foo 4) --> (0 2 4 6) hit every down beat.

    01234567
    X-X-X-X-


    (param foo (euclid 8 5))
    (next foo 5) --> (0 2 3 5 6) hit down-beats 1, 2 & 4, up-beats 2 & 3

    01234567
    X-XX-XX-
    

The shift argument produces the same rhythmic pattern but shifts the
selected notes

    (param foo (euclid 8 5 :shift 2))
    (next foo 5) --> (2 4 5 7 0)

    01234567
    X-X-XX-X

Use a hook function to convert the raw numeric output to a cue-list

    (param foo (euclid 8 5
                       :hook #'(lambda (n)
                                 (let ((sub (if (evenp n) 1 3))
                                       (beat (truncate (+ (* n 1/2) 1))))
                                   (list 1 beat sub)))))

    (next foo 5) --> ((1 1 1) (1 2 1) (1 2 3) (1 3 3) (1 4 1))" )

