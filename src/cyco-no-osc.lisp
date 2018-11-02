;;;; CYCO cyco-no-osc
;;;;
;;;; load CYCO without OSC support
;;;;

(defpackage :cyco
  (:use :cl))

(in-package :cyco)

(load "src/cyco-loader")

(constant +OSC-SUPPORTED+ nil)

(format t "CYCO loaded without OSC support~%")
