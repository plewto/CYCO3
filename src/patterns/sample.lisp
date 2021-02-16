;;;; CYCO patterns sample
;;;;
;;;; Sample and hold pattern
;;;;

(in-package :cyco)

(defclass sample-and-hold (slowglass) nil
  (:documentation
   "Samples source pattern on every nth call to next-1.
Unlike slowglass, the source pattern is advanced on each next-1 
call."))

(defun sample-and-hold (source &key (n 1))
  "(SAMPLE-AND-HOLD source &key n)

Samples source pattern on every nth call to next-1.
The source pattern is advanced on every next-1 call.

SAMPLE-AND-HOLD is similar to SLOWGLASS except that the order of 
the source pattern values may be rearranged."
  (reset (make-instance 'sample-and-hold
			:of (->list source)
			:divide (max 0 n))))

(defmethod clone ((mother sample-and-hold) &key &allow-other-keys)
  (sample-and-hold (clone (car (elements mother)))
		   :n (slowglass-rate mother)))


(defmethod next-1 ((snh sample-and-hold))
  (let ((source (car (elements snh))))
    (if (zerop (pointer snh))
	(progn 
	  (setf (pointer snh)
		(max 0 (1- (slowglass-rate snh))))
	  (setf (value snh)(next-1 source)))
      (prog1
	  (value snh)
	(setf (pointer snh)(1- (pointer snh)))
	(next-1 source)))))

(defmethod pattern-length ((snh sample-and-hold) &key (max 128) &allow-other-keys)
  (let ((p1 (pattern-length (car (elements snh)) :max max))
	(p2 (slowglass-rate snh)))
    (if (= p1 p2)
	p1
      (* p1 p2))))

	     

   
