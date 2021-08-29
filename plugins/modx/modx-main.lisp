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

(in-package :cyco)

(defun modx-program-map (bank-msb bank-lsb program-number)
  #'(lambda (time &key bank program)
      (let ((channel-index 0))
	(setf bank-lsb (or bank bank-lsb))
	(setf program-number (or program-number program))
	(list (cons time (midi-control-change channel-index 0 bank-msb))
	      (cons time (midi-control-change channel-index 32 bank-lsb))
	      (cons (+ time 0.05) (midi-program-change channel-index program-number))))))

(instrument +modx+
	    :parent +root-instrument+
	    :channel 1
	    :transient nil)

(param *current-modx-performance* nil)


(defmacro modx-performance (name bank-msb bank-lsb program-number)
  `(let ((inst (make-instrument ',name
				:parent +modx+
				:transient t)))
     (set-program-map inst (modx-program-map ,bank-msb ,bank-lsb ,program-number))
     ;;(put inst :program-number ,program-number)
     ;;(put inst :program-bank ,bank-msb)
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

(load-plugin-file "drumkits/real-brushes-kit")
(load-plugin-file "drumkits/iranian-mix-kit")
