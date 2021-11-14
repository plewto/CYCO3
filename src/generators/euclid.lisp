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

;; (defmethod value ((gen euclid))
;;   (funcall (slot-value gen 'value-hook)
;; 	   (mod (+ (slot-value gen 'shift)
;; 		   (round (slot-value gen 'internal-value)))
;; 		(slot-value gen 'length))))

(defmethod value ((gen euclid))
  (funcall (slot-value gen 'value-hook)
	   (+ (slot-value gen 'shift)
	      (round (slot-value gen 'internal-value)))))

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

TODO Redo Euclid documentation (remove value mod expression)


" )

