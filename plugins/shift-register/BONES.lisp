;;;; CYCO shift-register
;;;;
;;;; 

(constant +shift-register-length+ 16)

(defclass shift-register (pattern)
  ((length
    :allocation :class
    :type integer
    :reader sr-length
    :initform +shift-register-length+)
   (register
    :type integer
    :accessor sr-register
    :initform #b1
    :initarg :seed)
   (seed
    :type integer
    :reader sr-seed
    :initform #b1
    :initarg :seed)
   (taps
    :type integer
    :accessor sr-taps
    :initform #b1000000000000000
    :initarg :taps)
   (mask
    :type integer
    :accessor sr-mask
    :initform #b1111111111111111
    :initarg :mask)
   (base
    :type integer
    :accessor sr-base
    :initform #xffff
    :initarg :base)
   (bias
    :type initarg
    :accessor sr-bias
    :initform 0
    :initarg :bias)
   (prerun
    :type integer
    :accessor sr-prerun
    :initform 0
    :initarg :prerun))
  (:documentation
   "Implements a 16-bit linear feedback shift-register using binary
'cells', where any stage may be selected for feedback.  If there are an odd
number of true feedback stages, the feedback value is 1, otherwise it is 0.

Shift Register values have a tendency to blow-up producing very high
results.  The following function composition limits the results to a
manageable range.  

                     (bias (base (mask x)))

Where x is the 'nominal' register value.
mask - a bit-wise and 
base - output modulo, multiples of 12 are useful for key-number.
bias - fixed value added to result."))

(defmethod shift-register-p ((sr shift-register)) t)

(defmethod reset ((sr shift-register))
  (setf (sr-register sr)(sr-seed sr))
  (dotimes (i (sr-prerun sr))(next-1 sr))
  sr)

(defun shift-register (seed taps &key
			    (mask #b1111111111111111)
			    (base #xffff)
			    (bias 0)
			    (prerun 0))
  "Create new SHIFT-REGISTER object.
seed    - initial state 0 < seed <= #xffff.  The register consist of 16 
          stages.  The binary representation of seed, padded to the right
          with zeros, is the initial register state.  With each shift
          operation  the value is shifted to the right with bit 0 
          set to the feedback value.
taps    - Selects feedback taps.  The binary representation directly 
          indicates which stages are fed-back. 0 <= taps <= #xffff.
:mask   - mask bit-wise anded to register value. 0 <= mask <= #xffff
:base   - register value is modulo base, default 2 <= base <= #xffff
          The modulo operation is applied after the mask.
:bias   - value added to register.  Bias addition is applied after 
          the base.
:prerun - Number of times to step the register after it is created.
          For many configurations the register will progress through 
          an initial sequence of values before settling into a cyclical
          pattern.  prerun may be used to skip over these initial values." 
  (reset (make-instance 'shift-register
			:seed (logand seed #xffff)
			:taps (logand taps #xffff)
			:mask mask
			:base base
			:bias bias
			:prerun prerun)))

(defmethod clone ((sr shift-register) &key new-name new-parent)
  (dismiss new-name new-parent)
  (shift-register (sr-seed sr)
		  (sr-taps sr)
		  :mask (sr-mask sr)
		  :base (sr-base sr)
		  :bias (sr-bias sr)
		  :prerun (sr-prerun sr)))

(defmethod value ((sr shift-register))
  (let ((mask (sr-mask sr))
	(base (sr-base sr))
	(bias (sr-bias sr)))
    (+ (rem (logand mask (sr-register sr)) base) bias)))
    
(defmethod shift-register-feedback ((sr shift-register) &optional (insert 0))
  (let* ((a (sr-register sr))
	 (b (sr-taps sr))
	 (c (logand #xffff (+ (logand a b)(logand insert 1))))
	 (acc 0))
    (while (plusp c)
      (setf acc (+ acc (if (zerop (logand c 1)) 0 1)))
      (setf c (ash c -1)))
    (logand acc 1)))


(defmethod shift-1 ((sr shift-register) &optional (insert 0))
  (let ((fb (shift-register-feedback sr insert)))
    (setf (sr-register sr)
	  (logior (ash (sr-register sr) 1) fb)))
  (value sr))

(defmethod next-1 ((sr shift-register))
  (shift-1 sr 0))

(defmethod find-shift-register-period ((sr shift-register) &key 
				       (prerun 100)(guard 8000))
  (let ((seen (make-hash-table))
	(count 0))
    (dotimes (i prerun)(shift-1 sr))
    (while (< count guard)
      (let ((current (value sr)))
	(if (gethash current seen)
	    (return-from find-shift-register-period count))
	  (progn
	    (setf (gethash current seen) t)
	    (setf count (1+ count))
	    (shift-1 sr))))
    nil))

(defmethod dump-shift-register ((sr shift-register))
  (format t "SHIFT-REGISTER~%")
  (format t "    Register : ~A~%" (format-binary (sr-register sr)))
  (format t "    Taps     : ~A~%" (format-binary (sr-taps sr)))
  (format t "    Feedback : ~A~%" (format-binary (shift-register-feedback sr)))
  (format t "    Seed     : ~A~%" (format-binary (sr-seed sr)))
  (format t "    Mask     : ~A~%" (format-binary (sr-mask sr)))
  (format t "    Base     :  ~A~%" (sr-base sr))
  (format t "    Bias     :  ~A~%" (sr-bias sr))
  (format t "    Prerun   :  ~A~%" (sr-prerun sr)))

