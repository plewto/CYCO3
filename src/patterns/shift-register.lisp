;;;; CYCO shift-register
;;;;
;;;; 8-bit linear-feedback shift-register


(constant +shift-register-length+ 8)

(defclass shift-register (pattern)
  ((register
    :type integer
    :accessor shift-register-register
    :initform 1
    :initarg :seed)
   (seed
    :type integer
    :accessor shift-register-seed
    :initform 1
    :initarg :seed)
   (prerun
    :type integer
    :reader shift-register-prerun
    :initform 10
    :initarg :prerun)
   (taps
    :type integer
    :accessor shift-register-taps
    :initform 0
    :initarg :taps)
   (mask
    :type integer
    :accessor shift-register-mask
    :initform #b11111111
    :initarg :mask)
   (base
    :type integer
    :accessor shift-register-base
    :initform 10
    :initarg :base)
   (bias
    :type integer
    :accessor shift-register-bias
    :initform 0
    :initarg :bias)))

(defmethod shift-register-p ((sr shift-register)) t)

(defmethod reset ((sr shift-register))
  (setf (shift-register-register sr)
	(shift-register-seed sr))
  (dotimes (i (shift-register-prerun sr))(next-1 sr))
  sr)

(defun shift-register (seed taps &key (mask #b11111111)(base 12)(bias 0)(prerun 0))
  "Creates new SHIFT-REGISTER pattern
seed - initial value,  0 < seed <= #xff
taps - feedback taps,  0 <= taps <= #xff
mask - mask applied to output value. 0 < mask <= #xff
base - output value is modulo base.  0 < base
bias - amount added to output value
prerun - number of times to shift register after a reset or construction.

current value <-- ([register] and mask) mod base) + bias
"
  (reset (make-instance 'shift-register
			:seed (logand seed #xff)
			:prerun prerun
			:taps (logand taps #xff)
			:mask mask
			:base base
			:bias bias
			:prerun prerun)))
  
(defmethod clone ((sr shift-register) &key new-name new-parent)
  (dismiss new-name new-parent)
  (shift-register (shift-register-seed sr)
		  (shift-register-taps sr)
		  :mask (shift-register-mask sr)
		  :base (shift-register-base sr)
		  :bias (shift-register-bias sr)
		  :prerun (shift-register-prerun sr)))

(defmethod value ((sr shift-register))
  (let ((mask (shift-register-mask sr))
	(base (shift-register-base sr))
	(bias (shift-register-bias sr)))
    (+ (rem (logand mask (shift-register-register sr)) base) bias)))


(defmethod shift-register-feedback ((sr shift-register))
  (let ((acc 0)
	(reg (shift-register-register sr))
	(tap (shift-register-taps sr)))
    (dotimes (bit +shift-register-length+)
      (let ((fb (logand (logand reg 1)
			(logand tap 1))))
	(if (not (zerop fb))(setf acc (1+ acc)))
	(setf reg (ash reg -1)
	      tap (ash tap -1))))
    (logand acc 1)))
	    
(defmethod next-1 ((sr shift-register))
  (let ((fb (shift-register-feedback sr)))
    (setf (shift-register-register sr)
	  (logior (ash (shift-register-register sr) 1) fb)))
  (value sr))

(flet ((format-binary
	(byte)
	(let ((acc ""))
	  (dotimes (bit +shift-register-length+)
	    (let ((probe (expt 2 bit)))
	      (setf acc (str+ (if (zerop (logand probe byte)) "0" "1") acc))))
	  acc)))

  (defmethod dump-shift-register ((sr shift-register))
    (format t "SHIFT-REGISTER~%")
    (format t "Register : ~A~%" (format-binary (shift-register-register sr)))
    (format t "Taps     : ~A~%" (format-binary (shift-register-taps sr)))
    (format t "Feedback : ~A~%" (shift-register-feedback sr))
    (format t "Seed     : ~A~%" (format-binary (shift-register-seed sr)))
    (format t "Mask     : ~A~%" (format-binary (shift-register-mask sr)))))
