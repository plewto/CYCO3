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
    :initarg :prerun))
  (:documentation
   "LOGISTIC is a generator using the logistic map

    x1 = rx0(1 - x0)
 
    x0 and x1 are the current and next values respectively, 0.0 <= x0,x1 <= 1.0
    and r is a 'fecundity' parameter, 0 <= r <= 4.0, default r=3.25"))


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
  (setf (internal-value gen)
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
  (let* ((v0 (internal-value gen))
	 (v1 (* (logistic-mu gen) v0 (- 1 v0))))
    (setf (internal-value gen)
	  (if (funcall (monitor gen) v1)
	      (funcall (action gen) gen v1)
	    v1))
    (funcall (value-hook gen) v0)))

(defmethod pattern-length ((lg logistic) &key (max 128) &allow-other-keys)
  (reset lg)
  (let ((seen '())
	(v (internal-value lg)))
    (while (and (not (member v seen :test #'=))(plusp max))
      (push v seen)
      (next-1 lg)
      (setf v (internal-value lg))
      (setf max (1- max)))
    (length seen)))

(setf (documentation 'logistic 'function)
      "Returns new instance of LOGISTIC.

(logistic &key prerun seed mu hook monitor action)

:prerun  - Integer, number of times to increment map prior to use, default 0
:seed    - Float, initial value 0 <= seed <= 1, default 0.5
:hook    - Function applied by value method to internal value
           Default (lambda (n) n)
:monitor - Predicate called within next-1 to determine if action function
           should be called.  Default (lambda (value) nil)
:action  - Function called within next-1 whenever the monitor predicate 
           returns non-nil.  Default (lambda (logistic value) value)")
