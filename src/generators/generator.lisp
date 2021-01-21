;;;; CYCO generators/generator.lisp
;;;;


(in-package :cyco)

(defclass generator nil
  ((value-hook
    :type function
    :initform #'(lambda (n) n)
    :accessor value-hook
    :initarg :hook)
   (current-value
    :type number
    :initform 0
    :accessor current-value
    :initarg :seed))
  (:documentation
   "A Generator is a pattern like object for producing numeric sequences.
Unlike true patterns, generators may not contain nested elements, they 
may however be nested within patterns."))
  

(defmethod value ((gen generator))
  (funcall (slot-value gen 'value-hook)
	   (slot-value gen 'current-value)))

(defmethod reset ((gen generator)) gen)

(defmethod next-1 ((gen generator)) (value gen))


(defmethod next-n ((gen generator)(n integer))
  (loop for i from 1 to n
	collect (next-1 gen)))

(defmethod next ((gen generator) &optional (n 1))
  (cond ((= n 1)(next-1 gen))
	((integerp n)(next-n gen n))
	(t (value gen))))



(defclass constant-value (generator) nil)

(defun constant-value (n &key &allow-other-keys)
  (make-instance 'constant-value :seed n))

(defmethod clone ((mother constant-value) &key &allow-other-keys)
  (constant-value (value mother))) 



(defclass counter (generator) nil)

(defmethod reset ((gen counter))
  (setf (current-value gen) 0)
  gen)

(defun counter (&key (hook #'(lambda (n) n)) &allow-other-keys)
  (reset (make-instance 'counter :hook hook)))

(defmethod clone ((mother counter) &key &allow-other-keys)
  (let ((daughter (counter :hook (value-hook mother))))
    (setf (current-value daughter)(current-value mother))
    daughter))

(defmethod next-1 ((gen counter))
  (prog1
      (value gen)
    (setf (current-value gen)(1+ (current-value gen)))))


(defclass countdown (generator)
  ((initial-value
    :type integer
    :accessor initial-value
    :initform 16
    :initarg :seed)
   (action
    :type function
    :accessor countdown-action
    :initform #'(lambda (gen) gen)
    :initarg :action)
   (multi-trigger
    :type t
    :initform nil
    :initarg :multi-trigger)
   (has-fired
    :type t
    :initform nil)))

(defmethod reset ((gen countdown))
  (setf (current-value gen)(initial-value gen)
	(slot-value gen 'has-fired) nil)
  gen)

(defun countdown (n &key
		    (action #'(lambda (generator) generator))
		    (multi-trigger nil)
		    (hook #'(lambda (n) n)) &allow-other-keys)
  (reset (make-instance 'countdown :seed n
			:action action
			:multi-trigger multi-trigger
			:hook hook)))

(defmethod clone ((mother countdown) &key &allow-other-keys)
  (let ((daughter (countdown (initial-value mother)
			     :action (countdown-action mother)
			     :multi-trigger (slot-value mother 'multi-trigger)
			     :hook (value-hook mother))))
    (setf (current-value daughter)
	  (current-value mother))
    daughter))


(flet ((not-fired-p (gen)
		    (not (slot-value gen 'has-fired)))
       
       (multi-trigger-p (gen)
			(slot-value gen 'multi-trigger)))
  
  (defmethod next-1 ((gen countdown))
    (let* ((vout (value gen))
	   (v0 (current-value gen))
	   (v1 (max (1- v0) 0)))
      (setf (current-value gen) v1)
      (if (zerop v0)
	  (if (or (not-fired-p gen)(multi-trigger-p gen))
	      (progn
		(setf (slot-value gen 'has-fired) t)
		(funcall (countdown-action gen) gen))))
      vout)))


(setf (documentation 'constant-value 'function)
      "Returns generator with constant value
(param foo (constant-value x))
(next foo) --> x")



(setf (documentation 'counter 'function)
      "Returns generator which counts up from 0
(param foo (counter))
(next foo 5) --> (0 1 2 3 4)")

(setf (documentation 'countdown 'function)
      "Returns generator which counts down to 0.
An optional function my be executed when count reaches 0.

n       - Integer, initial value
:action - Function called once counter reaches 0.
          The function must have the form (lambda (gen) ) where gen 
          is this instance of countdown.  The action function is 
          used for its side-effects.
:hook   - Function applied to output value, default (lambda (n) n)
:multi-trigger - Boolean, if false the action function is executed 
                 only one time.  If true action is called every time
                 next-1 is called and the current value is 0.")
