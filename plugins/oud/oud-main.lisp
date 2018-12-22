;;;; CYCO oud plugin
;;;;
;;;; Oud defines a fretted instrument chord-model.
;;;; Classes and structures:
;;;;     CHORD-VARIATIONS - Holds all variations of specific chord-type
;;;;                        for a single key, IE all C Major chords.
;;;;     CHORD-FAMILY     - Holds all variations of specific chord-type
;;;;                        for all keys, IE all Major chords.
;;;;     MONOCHORD        - Defines key-range in terms of fret-positions
;;;;                        of a single string.
;;;;     POLYCHORD        - A suite of monochords.  POLYCHORD is an extension
;;;;                        of ABSTRACT-CHORD-MODEL.
;;;;

(defpackage :cyco-oud
  (:use :cl)
  (:import-from :cyco
                :+rest+
		:->list
		:->string
		:->symbol
		:->vector
		:abstract-chord-model
		:chord-table-descriptions
		:chord-template
		:chord-types
		:constant
		:copies
		:cyco-error
		:cyco-warning
		:defines-chord-p
		:dismiss
		:dump-chords
		:keyname
		:keynumber
		:keynumber-p
		:load-plugin-file
		:name
		:octave
		:param
		:pitch-class
		:rest-p
		:scopies
		:sformat
		:str+
		:while))

(in-package :cyco-oud)

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

(load-plugin-file "variations")
(load-plugin-file "family")
(load-plugin-file "monochord")
(load-plugin-file "polychord")
