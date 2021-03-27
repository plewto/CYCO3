;;;; CYCO modx plugin
;;;;
;;;; Provides instrument for Yamaha MODX structured as one main instrument
;;;; with optional parts.   The term "part" is used by Yamaha for modex
;;;; patch description and should not be confused with CYCO part's.
;;;;


(in-package :cyco)

(constant +MODX-BANKS+ '((:preset-1 64 0)
			 (:user-1 64 32)))


(param *current-modx-performance* nil)

(constant +modx-properties+
	  (append +instrument-properties+
		  '(:program-bank-lsb)))

(defclass modx-performance (instrument)
  ((channels
    :type vector
    :initform (->vector (copies 16))
    :accessor modx-channels))
  (:documentation "A specialized Instrument for Yamaha MODX performance.
The performance does not directly produce events.  Instead child
instruments should be added to correspond to each performance part/channel."))

(defun make-modx-performance (name bank program-number &key
				   remarks (parent *root-instrument*))
  "Creates new instance of modx-performance.
bank should be a symbol as listed in +MODX-BANKS+ association list.
program is a MIDI program number 1..128.

The new instance becomes the default for add-modx-part macro."
  (let* ((bank-values (or (cdr (assoc bank +modx-banks+))
	 		  (progn 
	 		    (cyco-warning (sformat "Unknowns MODX bank: ~A, using default" bank))
	 		    '(64 0))))
	 (bank-msb (car bank-values))
	 (bank-lsb (second bank-values))
	 (inst (make-instance 'modx-performance
			      :properties +modx-properties+
			      :name name 
			      :remarks remarks)))
    (connect parent inst)
    (if (not (and (plusp program-number)(<= program-number 128)))
	(progn
	  (cyco-warning (sformat "MODX program-number out of bounds: ~A, using default"
				 program-number))
	  (setf program-number 1)))
    (put inst :program-number (1- program-number))
    (put inst :program-bank bank-msb)
    (put inst :program-bank-lsb bank-lsb)
    (let ((pmap #'(lambda (time &key bank program)
		    (declare (ignore bank))
    		    (let* ((t0 (max 0 (- time 0.005)))
			   (t1 (+ t0 0.002))
			   (t2 (+ t0 0.004))
    			  (cindex 0))
    		      (cond ((eq program :doc)
    			     (format t "MODX ~A program ~A  bank-msb ~A  bank-lsb ~A"
    				     name (1+ program-number) bank-msb bank-lsb)
    			     '())
    			    ((or (eq program :default)(not program))
    			     (list (cons t0 (midi-control-change cindex 0 bank-msb))
    				   (cons t1 (midi-control-change cindex 32 bank-lsb))
    				   (cons t2 (midi-program-change cindex program-number))))
    			    (t
			     (list (cons t0 (midi-control-change cindex 0 bank-msb))
    				     (cons t1 (midi-control-change cindex 32 bank-lsb))
    				     (cons t2 (midi-program-change cindex program)))))))))
      (put inst :program-map pmap))
    (setf *current-modx-performance* inst)
    inst))
      

(defmacro modx-performance (name bank program-number &key remarks (parent *root-instrument*))
  "Creates new instance of modx-performance and bind it to the symbol name.
Otherwise modx-performance and make-modx-performance have identical usage."
  `(let ((inst (make-modx-performance ',name ,bank ,program-number
				      :remarks (or ,remarks "MODX Performance")
				      :parent ,parent)))
     (defparameter ,name inst)
     inst))

(defmacro add-modx-part (name channel &key
			      (modx *current-modx-performance*)
			      keynumber-map
			      articulation-map
			      dynamic-map
			      remarks)
  "Adds new part/channel to Yamaha modx-performance. 
The object bound to *current-modx-performance* is used by default.
The new instrument is bound to the symbol name."
  `(let ((inst (make-instrument ',name
				:channel ,channel
				:keynumber-map ,keynumber-map
				:articulation-map ,articulation-map
				:dynamic-map ,dynamic-map
				:remarks (sformat "~A" (or ,remarks "")))))
     (connect ,modx inst)
     (put inst :program-map nil)
     (setf (aref (modx-channels ,modx)(1- ,channel)) inst)
     (defparameter ,name inst)
     inst))

