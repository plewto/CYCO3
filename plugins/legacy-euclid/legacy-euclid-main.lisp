;;;; CYCO plugin legacy-euclid
;;;;
;;;; Generates Euclid pattern.
;;;; https://en.wikipedia.org/wiki/Euclidean_rhythm
;;;; https://dbkaplun.github.io/euclidean-rhythm/
;;;;
;;;; Moved from cyco3 euclid generator to plugin 2022-06-06
;;;;

(in-package :cyco)

(defclass legacy-euclid (generator)
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
   "LEGACY-EUCLID is a generator for producing 'Euclidean Rhythms'
https://en.wikipedia.org/wiki/Euclidean_rhythm"))


(defmethod reset ((e legacy-euclid))
  (setf (internal-value e) 0)
  e)

(defun legacy-euclid (length points &key (shift 0)
		      (hook #'(lambda (n) n)))
  (if (not (and (numberp length)(plusp length)))
      (cyco-error (sformat "Euclid length argument must be positive number, got ~A" length)))
  (if (not (and (numberp points)))
      (cyco-error (sformat "Euclid points argument must be positive number, got ~A" points)))
  (reset (make-instance 'legacy-euclid
			:length length
			:points points
			:increment (/ length points)
			:shift shift
			:hook hook)))

(defmethod clone ((mother legacy-euclid) &key &allow-other-keys)
  (legacy-euclid (slot-value mother 'length)
	  (slot-value mother 'points)
	  :shift (slot-value mother 'shift)
	  :hook (value-hook mother)))

(defmethod value ((gen legacy-euclid))
  (funcall (slot-value gen 'value-hook)
	   (+ (slot-value gen 'shift)
	      (round (slot-value gen 'internal-value)))))

(defmethod next-1 ((gen legacy-euclid))
  (prog1
      (value gen)
    (setf (internal-value gen)
	  (+ (internal-value gen)
	     (slot-value gen 'increment)))))
	  

;; NOTES: Returns incorrect value when points > length.
;;
(defmethod pattern-length ((gen legacy-euclid) &key &allow-other-keys)
  (let* ((guard 5000)
	 (seen '())
	 (points 0)
	 (gen (clone gen))
	 (value 0))
    (while (not (and (member value seen)(< points guard)))
      (push value seen)
      (setf value (next-1 gen))
      (setf points (1+ points)))
    (1- points)))


(defmethod cue-function ((gen legacy-euclid) &key division)
  (let ((fn (cue-n (or division (slot-value gen 'length)))))
    #'(lambda (time-signature point)
	(funcall fn time-signature (list 0 point)))))




(setf (documentation 'legacy-euclid 'function)
      "Creates 'Euclidean Rhythm' Generator.

https://en.wikipedia.org/wiki/Euclidean_rhythm
https://dbkaplun.github.io/euclidean-rhythm/

Legacy-Euclid generates values by dividing length as evenly as possible into the
given number of points. 

LENGTH   - Integer length.  2 <= length
POINTS   - Number of points length is divided by.
           Most Euclidean algorithms restricts points to be less then or
           equal to length.  This implementation allows values greater
           then the length.  In theses cases duplicate values are
           generated. 

           (next (legacy-euclid 4 5) 5) --> (0 1 2 2 3)

:SHIFT   - Offset added to all values.
           (next (legacy-euclid 4 3) 3) --> (0 1 3)
           (next (legacy-euclid 4 3 :shift 10)) --> (10 11 13)

:HOOK    - Function applied to each value
           (next (legacy-euclid 4 3 :hook #'(lambda (n)(* 100 n))) 3) --> (0 100 300)
          
The Euclidean results are particularly useful for generating rhythmic
cue-list.   For example, assuming 4/4 time, a length of 16 may be treated 
as the 16th notes of a bar (staring with 0).  The generated results make a
selection of which 16th notes are played.


Calling the CUE-FUNCTION method on an instance of Euclid returns a
cuing-function specifically for that instance.


(let* ((e (legacy-euclid 8 3)))
    (qball foo snare
       :bars 1
       :cuefn (cue-function e)
       :cue (next e 3)
       :key 'hit))")


