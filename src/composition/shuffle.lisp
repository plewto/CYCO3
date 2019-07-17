;;;; CYCO
;;;;


(let ((docstring
       "A shuffle function is a function which selectively returns a time
shift (in seconds) based on specific argument combinations.   

The arguments to a shuffle function should have the same format as
the cue function currently in use. 

The default no-shuffle function always returns 0.0

Currently shuffle only applies to QBALL parts, but may be set
at either the QBall, Section or Project level."))
       
  (defun no-shuffle (time-specification)
    (dismiss time-specification)
    0.0)
  (setf (documentation 'null-shuffle-function 'function) docstring))



