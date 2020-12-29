

(defpackage :cyco-sr
  (:use :cl)
  (:import-from :cyco
		:clone
		:constant
		:format-binary
		:load-plugin-file
		:next-1
		:pattern
		:value
		:while))

(in-package :cyco-sr)

(defgeneric dump (object))
(defgeneric shift-register-p (object))
(defmethod shift-rgister-p ((object t)) nil)

(defgeneric shift-register-feedback (shift-register &optional insert))
(defgeneric shift-1 (shift-register &optional insert))
(defgeneric shift-register-period (shift-register &key prerun guard))


(load-plugin-file "bsr")

(export '(bsr
	  dump
	  shift-register-p
	  shift-register-feedback
	  shift-1
	  shift-register-period)
	:cyco-sr)

(import '(cyco-sr:shift-register-p
	  bsr)  :cyco)

(in-package :cyco)

