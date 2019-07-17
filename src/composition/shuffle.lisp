;;;; CYCO
;;;;

(let ((docstring
       "ISSUE: Write shuffle function documentation."))


  ;; Returns time offset in seconds.
  (defun no-shuffle (time-specification)
    (dismiss time-specification)
    0.0)
  (setf (documentation 'null-shuffle-function 'function) docstring))



