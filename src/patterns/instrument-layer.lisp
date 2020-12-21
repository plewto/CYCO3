;;;; CYCO pattern instrument-layer.lisp
;;;;
;;;; An INSTRUMENT-LAYER is a special-case pattern for handling
;;;; list of instruments within a part.
;;;;

(in-package :cyco)

(defclass instrument-layer (pattern) nil
  (:documentation 
"A INSTRUMENT-LAYER is a specialized Pattern which always returns
a list of all of its elements.

Use Case:
   Instrument-Layer is only used in conjunction with Parts to provide
   a layering rendering mode.   All instruments in an instrument-layer
   respond to the same events in parallel."))
   
(defun instrument-layer (&key (of '()))
  "Creates new instance of instrument-layer pattern."
  (make-instance 'instrument-layer :of (->list of)))

(defmethod next-1 ((q instrument-layer))
  (elements q))

(defmethod next-n ((q instrument-layer)(_ t))
  (declare (ignore _))
  (next-1 q))

(defmethod next ((q instrument-layer) &optional (_ nil))
  (declare (ignore _))
  (next-1 q))
		 
(defmethod instrument-layer-p ((object instrument-layer)) t)

(defmethod remaining ((object instrument-layer))
  (length (elements object)))



