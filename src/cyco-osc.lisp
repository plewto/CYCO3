;;;; CYCO cyco-osc
;;;;
;;;; Load CYCO with OSC support
;;;;

(load "~/quicklisp/setup")

(eval-when (:execute)
  (ql:quickload :osc)
  (ql:quickload :usocket))


(defpackage :cyco
    (:use :cl :osc :usocket))

(in-package :cyco)

(load "src/cyco-loader")

(constant +OSC-SUPPORTED+ t)

(format t "CYCO loaded with OSC support~%")

  
