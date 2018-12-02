;;;; CYCO fretworks plugin
;;;;
;;;; Fretted Instrument Chord Model
;;;;

(defpackage :cyco-fretworks
  (:use :cl)
  (:import-from :cyco
                :+rest+
                :->list
                :->symbol
                :->vector
		:->string
                :abstract-chord-model
                :copies
                :cyco-error
                :cyco-warning
                :dismiss
                :dump-chords
                :keyname
                :keynumber
                :keynumber-p
                :load-plugin-file
                :octave
                :pitch-class
                :rest-p
                :sformat
                :while
		:name
		:constant
		
		:param
		))

(in-package :cyco-fretworks)


;; Returns mean keynumber of chord template, ignoring rest.
;;
(defun mean-keynumber (lst)
  (let* ((ulst (remove nil lst :test #'(lambda (a b)(dismiss a)(rest-p b))))
	 (sum (apply #'+ (keynumber ulst)))
	 (count (length ulst)))
    (if (zerop count)
	0
      (/ (float sum) count))))
      

;; eq symbol test ignoring package.
;;
(defun symbol-eq-p (a b)
  (eq (->symbol a :cyco)
      (->symbol b :cyco)))

(load-plugin-file "chord-variations")
(load-plugin-file "chord-family")
(load-plugin-file "monochord")
(load-plugin-file "fretted-chord-model")
(load-plugin-file "bass-guitar/bass-guitar")

(export '(fretted-chord-model
	  define-chord-family
	  +bass-guitar-chord-model+
	  )
	:cyco-fretworks)

;; Import fretworks symbols to main :CYCO package
;;
(import '(cyco-fretworks:fretted-chord-model
	  cyco-fretworks:define-chord-family
	  cyco-fretworks:+bass-guitar-chord-model+
	  ) :cyco)

(in-package :cyco)
