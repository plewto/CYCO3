;;;; CYCO3 src/patterns/instrument-layer
;;;;
;;;; A Instrument-Layer is a highly specilized Pattern which always returns
;;;; a list of all of it's elements.
;;;;
;;;; Use Case:
;;;;    Instrument-Layer is only used in conjunction with Parts to provide
;;;;    a layering rendering mode.   All instruments in an instrument-layer
;;;;    respond to the same events in parallel.

(defclass instrument-layer (pattern) nil)

(defun instrument-layer (&key (of '()))
  (make-instance 'instrument-layer :of (->list of)))

(defmethod next-1 ((q instrument-layer))
  (elements q))

(defmethod next-n ((q instrument-layer)(_ t))
  (dismiss _)
  (next-1 q))

(defmethod next ((q instrument-layer) &optional (_ nil))
  (dismiss _)
  (next-1 q))
		 
(defgeneric instrument-layer-p (obj))
(defmethod instrument-layer-p ((obj t)) nil)
(defmethod instrument-layer-p ((obj instrument-layer)) t)

(defmethod remaining ((obj instrument-layer))
  (length (elements obj)))

