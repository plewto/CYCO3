;;;; CYCO generators/logistic.lisp
;;;;
;;;; Generator based on Logistic map
;;;; https://en.wikipedia.org/wiki/Logistic_map
;;;;

(in-package :cyco)

(defclass logistic-generator (generator)
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

(defun logistic-generator (&key (prerun 0)(seed 0.5)(mu 3.25)(hook #'(lambda (n) n)) &allow-other-keys)
  (reset (make-instance 'logistic-generator
			:prerun prerun
			:seed seed
			:mu mu
			:hook hook)))
   

(defmethod reset ((gen logistic-generator))
  (setf (current-value gen)
	(slot-value gen 'seed))
  (dotimes (i (logistic-prerun gen))
    (next-1 gen))
  gen)

(defmethod clone ((mother logistic-generator) &key new-name new-parent)
  (declare (ignore new-name new-parent))
  (let ((daughter (logistic-generator
		   :prerun (logistic-prerun mother)
		   :seed (slot-value mother 'seed)
		   :mu (logistic-mu mother)
		   :hook (value-hook mother))))
    (setf (current-value daughter)
	  (current-value mother))
    daughter))

(defmethod next-1 ((gen logistic-generator))
  (let* ((v0 (current-value gen))
	 (v1 (* (logistic-mu gen) v0 (- 1 v0))))
    (setf (current-value gen) v1)
    v0))
      
(setf (documentation 'logistic-generator 'function)
      "Returns a generator which produces values using a logistic-map.
https://en.wikipedia.org/wiki/Logistic_map

:prerun - Integer number of time to step the generator after it is created or reset, 
          default 0.
:seed   - Float, initial value 0.0 <= seed <= 1.0, default 0.5
:mu     - Float, 'fecundity' value 0.0 <= mu <= 4.0, default 3.25
:hook   - Function, value hook, default (lambda (n) n)")
