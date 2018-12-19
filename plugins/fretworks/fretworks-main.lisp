;;;; CYCO fretworks plugin
;;;;
;;;; Fretted Instrument Chord Model
;;;;

(defpackage :cyco-fretworks
  (:use :cl)
  (:import-from :cyco
                :+rest+
                :->list
                :->string
                :->symbol
                :->vector
                :abstract-chord-model
                :chord-template
		:chord-types
                
                :copies
                :cyco-error
                :cyco-warning
                :dismiss
		:defines-chord-p
                :dump-chords
                :keyname
                :keynumber
                :keynumber-p
                :load-plugin-file
                :name
                :octave
                :pitch-class
                :rest-p
                :sformat
                :while
		:param

		:constant
		))

(in-package :cyco-fretworks)


(defun ->cyco-symbol (sym)
  (->symbol (string-upcase (->string sym)) :cyco))

;; eq symbol test ignoring package.
;;
(defun symbol-eq-p (a b)
  (eq (->cyco-symbol a)
      (->cyco-symbol b)))

;; Returns mean keynumber of chord template, ignoring rest.
;;
(defun mean-keynumber (lst)
  (let* ((ulst (remove nil lst :test #'(lambda (a b)(dismiss a)(rest-p b))))
	 (sum (apply #'+ (keynumber ulst)))
	 (count (length ulst)))
    (if (zerop count)
	0
      (/ (float sum) count))))
      



(load-plugin-file "chord-variations")
(load-plugin-file "chord-family")
(load-plugin-file "monochord")
(load-plugin-file "fretted-chord-model")
(load-plugin-file "guitar")
(load-plugin-file "bass-guitar")
(load-plugin-file "ukulele")

(export '(fretted-chord-model
	  define-chord-family
	  *guitar-chord-model*
	  *bass-guitar-chord-model*
	  *ukulele-chord-model*
	  )
	:cyco-fretworks)

;; Import fretworks symbols to main :CYCO package
;;
(import '(cyco-fretworks:fretted-chord-model
	  cyco-fretworks:define-chord-family
	  cyco-fretworks:*guitar-chord-model*
	  cyco-fretworks:*bass-guitar-chord-model*
	  cyco-fretworks:*ukulele-chord-model*
	  ) :cyco)

(in-package :cyco)
