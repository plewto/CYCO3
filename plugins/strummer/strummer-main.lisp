;;;; CYCO Strummer Plugin
;;;;

(defpackage cyco-strummer
  (:use :cl)
  (:import-from :cyco
		:chord-model
		:load-plugin-file
		:+REST+
		:octave
		:final
		:->vector
		:copies
		:pitch-class
		:mean
		:?
		:->string
		:dismiss
		:sformat
		:while
		:->symbol
		:keyname
		:->list
		:str+
		:cyco-warning
		:keynumber
		:constant
		;; :->list
		;; :->symbol
		;; :dismiss
                ;; :keynumber-p
                ;; 
		;; :dump-chords
                ;; :name
                ;; :octave
                ;; :rest-p
		;; :keynumber
		;; :octave
		;; :pitch-class
		;; :keyname
		;; :param
		;; :while
		;; :copies
		;; :chord-model
		))
		
(in-package :cyco-strummer)


(load-plugin-file "chord-family")
(load-plugin-file "monochord")
(load-plugin-file "fret-model")
(load-plugin-file "guitar")

(format t "~A~%" (chord +guitar-chords+ '[maj] 'c3))



(in-package :cyco)

