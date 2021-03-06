;;;; CYCO generators/shift-register.lisp
;;;;
;;;; Generator based on line-feedback shift-register
;;;; https://en.wikipedia.org/wiki/Linear-feedback_shift_register

(in-package :cyco)

(defclass shift-register (generator)
  ((seed
    :type integer
    :accessor shift-register-seed
    :initform #b1
    :initarg :seed
    :documentation
    "Initial register value.")
   (taps
    :type integer
    :accessor shift-register-taps
    :initform #b1000000000000000
    :initarg :taps
    :documentation
    "High bits selects which register cells contribute to feedback.")
   (mask
    :type integer
    :reader shift-register-mask
    :initform #b1111111111111111
    :initarg :mask
    :documentation
    "Mask and to register's value, effectively setting the register's length.")
   (prerun-count
    :type integer
    :accessor shift-register-prerun
    :initform 0
    :initarg :prerun
    :documentation
    "Number of times to step register after a reset."))
  (:documentation
   "SHIFT-REGISTER is a generator using a linear-feedback shift-register.
https://en.wikipedia.org/wiki/Linear-feedback_shift_register"))
   
(defun shift-register (seed taps &key
			    mask
			    (prerun 0)
			    (hook #'(lambda (n) n))
			    &allow-other-keys)
  (reset (make-instance 'shift-register
			:hook hook
			:seed seed
			:taps taps
			:mask (or mask #b1111111111111111)
			:prerun prerun)))

(defmethod reset ((sr shift-register))
  (setf (internal-value sr)(shift-register-seed sr))
  (dotimes (i (shift-register-prerun sr))(next-1 sr))
  sr)


(defmethod clone ((mother shift-register) &key &allow-other-keys)
  (shift-register (shift-register-seed mother)
		  (shift-register-taps mother)
		  :mask (shift-register-mask mother)
		  :prerun (shift-register-prerun mother)
		  :hook (value-hook mother)))


(defmethod shift-register-feedback ((sr shift-register))
  (let* ((mask (shift-register-mask sr))
	 (v (logand (internal-value sr) mask))
	 (taps (logand (shift-register-taps sr) mask)))
    (parity (logand v taps))))

(defmethod next-1 ((sr shift-register))
  (let* ((mask (shift-register-mask sr))
	 (fb (shift-register-feedback sr))
	 (v0 (internal-value sr))
	 (v1 (logior (ash v0 1) fb)))
    (prog1
	(value sr)
      (let ((v-next (logand v1 mask)))
	(setf (internal-value sr) v-next)))))

(defmethod pattern-length ((sr shift-register) &key (max 128) &allow-other-keys)
  (reset sr)
  (if (zerop (shift-register-prerun sr))
      (next sr 100))
  (let ((seen '())
	(v (internal-value sr)))
    (while (and (plusp max)(not (member v seen :test #'=)))
      (push v seen)
      (next-1 sr)
      (setf v (internal-value sr))
      (setf max (1- max)))
    (reset sr)
    (length seen)))

(defmethod ? ((sr shift-register))
  (format t "SHIFT-REGISTER~%")
  (format t "Mask      ~A~%" (format-binary (shift-register-mask sr)))
  (format t "Register  ~A  decimal ~4D~%" (format-binary (internal-value sr))(internal-value sr))
  (format t "Taps      ~A~%" (format-binary (shift-register-taps sr)))
  (format t "Feedback  ~A~%" (shift-register-feedback sr)))


(setf (documentation 'shift-register 'function)
      "Creates new SHIFT-REGISTER generator.

(shift-register seed taps &key mask prerun hook)

seed    - Integer, initial register state,  seed > 0.
          If seed is expressed in binary it directly reflects the 
          initial register state. 
taps    - Integer, feedback taps, taps >= 0.
          If taps is expressed in binary it directly reflects which 
          stages contribute to feedback.  At a minimum the taps value
          usually has the high bit set.
:mask   - Integer, mask anded to register value and feedback-taps.
          The mask value may be used to set the register's length.
          Default #b1111111111111111
:prerun - Integer, number of times to step the register after a reset
          or after it is constructed.  It is often the case that a 
          shift-register Will produce spurious values before settling
          into a periodic mode.  The prerun value may be used to skip over
          these initial spurious values, default 0.           
:hook   - Function applied to the 'raw' register value.  Shift register
          outputs have a tendency to 'blow up' and produce very high 
          values.  The hook function may be used to reign in excessive 
          register values.  Defaults to identity (lambda n) --> n.")
