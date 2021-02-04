;;;; CYCO generators/ramp.lisp
;;;;
;;;; ramp vs counter
;;;;
;;;;            ramp                counter
;;;; type    :  float               integer only
;;;; monitor :  (lambda (value))    (lambda (value))
;;;; action  :  (lambda (ramp))     (lambda (counter value))
;;;;              Does not update     Directly updates
;;;;              current-value       current-value.
          

(in-package :cyco)

(defclass ramp (generator)
  ((floor
    :type number
    :accessor ramp-floor
    :initform 0
    :initarg :floor)
   (ceiling
    :type number
    :accessor ramp-ceiling
    :initform 9
    :initarg :ceiling)
   (increment
    :type number
    :accessor ramp-increment
    :initform 1
    :initarg :increment)))


(defun ramp (a b &key (by 1)
	       (hook #'(lambda (n) n))
	       (monitor #'(lambda (v)
			    (declare (ignore v))
			    nil))
	       (action #'(lambda (ramp)
			   (declare (ignore ramp))))
	       &allow-other-keys)
  (make-instance 'ramp
		 :hook hook
		 :monitor monitor
		 :action action 
		 :seed a
		 :floor a
		 :ceiling b
		 :increment (if (< a b)
				(abs by)
			      (* -1 (abs by)))))

(defmethod reset ((ramp ramp))
  (setf (current-value ramp)(ramp-floor ramp))
  ramp)

(defmethod clone ((mother ramp) &key &allow-other-keys)
  (ramp (ramp-floor mother)
	(ramp-ceiling mother)
	:by (ramp-increment mother)
	:monitor (monitor mother)
	:action (action mother)
	:hook (value-hook mother)))


(flet ((increment (ramp ceiling)
		  (let* ((delta (ramp-increment ramp))
			 (v0 (current-value ramp))
			 (v1 (min ceiling (+ delta v0))))
		    (setf (current-value ramp) v1)))
       
       (decrement (ramp ceiling)
		  (let* ((delta (ramp-increment ramp))
			 (v0 (current-value ramp))
			 (v1 (max ceiling (+ delta v0))))
		    (setf (current-value ramp) v1))) )
  
  (defmethod next-1 ((ramp ramp))
    (prog1
	(progn
	  (if (funcall (monitor ramp)(current-value ramp))
	      (funcall (action ramp) ramp))
	  (value ramp))
      (let ((floor (ramp-floor ramp))
	    (ceiling (ramp-ceiling ramp)))
	(cond ((< floor ceiling)(increment ramp ceiling))
	      ((> floor ceiling)(decrement ramp ceiling))
	      (t nil))))) )


;; TODO Update ramp docstring

(setf (documentation 'ramp 'function)
      "A RAMP generator creates a linear sequence of numbers.

a     - Initial value
b     - Final value
:by   - Increment. The increment sign is automatically adjusted to match
        the slope determined by a and b.  Default 1 or -1
:hook - Function (lambda (n)) --> n2  applied to 'raw' ramp value.
        Defaults to an identity.
:monitor - Function  (lambda value) --> bool  
           When monitor returns true, call action function.
           Default (lambda (value) nil)
:action  - Function (lambda (ramp))
           Called whenever the monitor function returns true
           for the ramp's current-value
           Default (lambda (ramp))
           

Examples

   (next (ramp 0 9) 12) --> (0 1 2 3 4 5 6 7 8 9 9 9)

   (next (ramp 9 0) 12) --> (9 8 7 6 5 4 3 2 1 0 0 0)

   (next (ramp 0 9 :by 2) 12) --> (0 2 4 6 8 9 9 9 9 9 9 9)

   (next (ramp 9 0 :by 2) 12) --> (9 7 5 3 1 0 0 0 0 0 0 0)

   a, b and by values need not be integers

   (next (ramp 0 9 :by 1.5) 12) --> (0 1.5 3.0 4.5 6.0 7.5 9 9 9 9 9 9)

   Use of hook function

   (next (ramp 0 4 :hook #'(lambda (n)(+ 100 n))) 6) --> (100 101 102 103 104 104)")



