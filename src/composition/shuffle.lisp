;;;; CYCO composition
;;;;
;;;; Defines default shuffle function.


(in-package :cyco)

(let ((docstring
       "A shuffle function selectively returns a time
shift (in seconds) based on specific argument combinations.   

The arguments to a shuffle function should have the same format as
the cue function currently in use. 

The default no-shuffle function always returns 0.0"))
       
  (defun no-shuffle (time-specification)
    (dismiss time-specification)
    0.0)
  (setf (documentation 'null-shuffle-function 'function) docstring))

