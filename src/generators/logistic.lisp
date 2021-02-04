;;;; CYCO generators/logistic.lisp
;;;;
;;;; Generator based on Logistic map
;;;; https://en.wikipedia.org/wiki/Logistic_map
;;;;

(in-package :cyco)

(defclass logistic (generator)
  ((seed
    :type float
    :initform 0.5
    :initarg :seed)
   (mu
    :type float
    :initform 2.0
    :accessor logistic-mu
    :initarg :mu)
   (prerun-count
    :type integer
    :accessor logistic-prerun
    :initform 0
    :initarg :prerun)))

(defun logistic (&key (prerun 0)(seed 0.5)(mu 3.25)
		      (hook #'(lambda (n) n))
		      (monitor #'(lambda (value)
				   (declare (ignore value))
				   nil))
		      (action #'(lambda (logistic value)
				  (declare (ignore logistic))
				  value))
		      &allow-other-keys)
  (reset (make-instance 'logistic
			:prerun prerun
			:seed seed
			:mu mu
			:monitor monitor
			:action action
			:hook hook)))

(defmethod reset ((gen logistic))
  (setf (current-value gen)
	(slot-value gen 'seed))
  (dotimes (i (logistic-prerun gen))
    (next-1 gen))
  gen)

(defmethod clone ((mother logistic) &key &allow-other-keys)
  (logistic
   :prerun (logistic-prerun mother)
   :seed (slot-value mother 'seed)
   :mu (logistic-mu mother)
   :monitor (monitor mother)
   :action (action mother)
   :monitor (monitor mother)
   :action (action mother)
   :hook (value-hook mother)))

(defmethod next-1 ((gen logistic))
  (let* ((v0 (current-value gen))
	 (v1 (* (logistic-mu gen) v0 (- 1 v0))))
    (setf (current-value gen)
	  (if (funcall (monitor gen) v1)
	      (funcall (action gen) gen v1)
	    v1))
    (funcall (value-hook gen) v0)))


;; TODO update logistic docstring

(setf (documentation 'logistic 'function)
      "Returns a generator which produces values using a logistic-map.
https://en.wikipedia.org/wiki/Logistic_map

:prerun - Integer number of time to step the generator after it is created or reset, 
          default 0.
:seed   - Float, initial value 0.0 <= seed <= 1.0, default 0.5
:mu     - Float, 'fecundity' value 0.0 <= mu <= 4.0, default 3.25
:hook   - Function, value hook, default (lambda (n) n)")

