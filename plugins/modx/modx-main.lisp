;;;; CYCO modx plugin  
;;;;
;;;; For Yamaha MODX Instruments.
;;;;
;;;; +ROOT-INSTRUMENT+
;;;;   |
;;;;   +-- +MODX+
;;;;         |
;;;;         +-- modx-performance (name bank-lsb bank-msb program-number)
;;;;               |
;;;;               +-- modx-instrument (channel 1)
;;;;               +-- modx-instrument (channel 2)
;;;;               +--
;;;;               +-- modx-instrument (channel 16)
;;;;
;;;; 1) Create a modx-performance instrument.
;;;;    Use this for program-change events and as parent to
;;;;    specific instruments.  The modx-performance macro
;;;;    binds the new performance to *current-modx-performance*
;;;;
;;;; 2) Use modx-instrument to create 1 or more instruments under
;;;;    the modx-performance, each on specific MIDI channel.
;;;;    The performance keyword argument to modx-instrument
;;;;    defaults to *current-modx-performance*


(defpackage :modx
  (:use :cl)
  (:import-from :cyco
		:->string
		:+root-instrument+
		:instrument
		:make-instrument
		:set-program-map
		:midi-control-change
		:midi-program-change
		:basic-keynumber-map
		:basic-dynamic-map
		:basic-articulation-map
		:symbolic-keynumber-map
		:external-load-plugin-file
		:extract-sub-symbolic-keylist
		))

(in-package :modx)

(defparameter *current-modx-performance* nil)

(defun modx-program-map (bank-msb bank-lsb program-number)
  #'(lambda (time &key bank program)
      (declare (ignore bank program))
      (let ((channel-index 0))
	(list (cons time (midi-control-change channel-index 0 bank-msb))
	      (cons time (midi-control-change channel-index 32 bank-lsb))
	      (cons (+ time 0.05) (midi-program-change channel-index program-number))))))

(instrument +modx+
	    :parent +root-instrument+
	    :channel 1
	    :transient nil)

(defmacro modx-performance (name bank-msb bank-lsb program-number)
  `(let ((inst (make-instrument ',name
				:parent +modx+
				:transient t)))
     (set-program-map inst (modx-program-map ,bank-msb ,bank-lsb ,program-number))
     (defparameter ,name inst)
     (setf *current-modx-performance* inst)
     inst))


(defmacro modx-instrument (name channel &key
				(performance nil)
				(keynumber-map (basic-keynumber-map))
				(dynamic-map (basic-dynamic-map))
				(articulation-map (basic-articulation-map))
				(remarks ""))
  `(let ((inst (make-instrument ',name
				:parent (or ,performance *current-modx-performance*)
				:channel ,channel
				:keynumber-map ,keynumber-map
				:dynamic-map ,dynamic-map
				:articulation-map ,articulation-map
				:transient t
				:remarks (->string (or ,remarks "")) )))
     (defparameter ,name inst)
     inst))

(defun ?modx ()
  (format t ";; MODX MIDI Parameters:~%")
  (format t ";;    000 Bank Select MSB~%")
  (format t ";;    001 Wheel~%")
  (format t ";;    002 Breath~%")
  (format t ";;    007 Volume~%")
  (format t ";;    011 Foot (Expression)~%")
  (format t ";;    016 Ribbon~%")
  (format t ";;    017 Assign knob 1~%")
  (format t ";;    018 Assign knob 2~%")
  (format t ";;    019 Assign knob 3~%")
  (format t ";;    020 Assign knob 4~%")
  (format t ";;    021 Assign knob 5~%")
  (format t ";;    022 Assign knob 6~%")
  (format t ";;    023 Assign knob 7~%")
  (format t ";;    024 Assign knob 8~%")
  (format t ";;    032 Bank Select LSB~%")
  (format t ";;    086 Switch 1~%")
  (format t ";;    087 Switch 2~%")
  (format t ";;    088 MS Hold~%")
  (format t ";;    089 MS Trigger~%")
  (format t ";;    092 Scene CC~%")
  (format t ";;    095 Super Knob~%"))


(modx-performance default-modx 0 0 0)

(export '(*current-modx-performance*
	  modx-performance
	  modx-instrument
	  ?modx) :modx)

(import '(modx:*current-modx-performance*
	  modx:modx-performance
	  modx:modx-instrument
	  ?modx) :cyco)
	  
 
