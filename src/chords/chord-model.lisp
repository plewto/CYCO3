;;;; CYCO
;;;;


(defclass chord-model nil nil
  (:documentation
   "A CHORD-MODEL provides named chord templates to a project.
Each level of a project may have it's own chord-model or it may 
inherit one from it's parent.  A project automatically inherits the 
the chord-model bound to *CHORD-TABLE*.

The reason for this level of abstraction is to provide better realism 
when using fretted instrument simulations.   The default chord-model
takes basically a keyboard's view.  Currently (28 Oct 2018) no 
other chord models are defined."))

(defmethod chord-model-p ((obj chord-model)) t)

;; Transformations on chords
;;

(flet ((rotate-right ;; (A B C) --> (B C A')
	(key-list)
	(let ((head (keynumber (car key-list)))
	      (tail (keynumber (cdr key-list))))
	  (if (not (rest-p head))
	      (progn
		(setf head (+ 12 head))
		(while (> head 127)(setf head (- head 12)))))
	  (append tail (list head))))
       (rotate-left ;; (A B C) --> (C' A B)
	(key-list)
	(let ((tail (keynumber (final key-list)))
	      (head (keynumber (butfinal key-list))))
	  (if (not (rest-p tail))
	      (progn
		(setf tail (- tail 12))
		(while (minusp tail)(setf tail (+ tail 12)))))
	  (cons tail head))))

  ;; add-octave <-- integer 
  (defun chord-inversion (template degree &key add-octave)
    "Apply inversion to chord template.
template - A list of keynumber offsets.
degree - inversion degree.
:add-octave - flag, if true repeat initial note (after inversion)
one octave higher at the end the template list.

(chord-inversion '(0 4 7)  0)    --> (0 4 7)    major triad in root position
(chord-inversion '(0 4 7)  1)    --> (4 7 12)   first inversion
(chord-inversion '(0 4 7)  2)    --> (7 12 16)  second inversion
(chord-inversion '(0 4 7)  4)    --> (12 16 19) 
(chord-inversion '(0 4 7) -1)    --> (-5 0 4)
(chord-inversion '(0 4 7) 0 :add-octave t)  --> (0 4 7 12)
(chord-inversion '(0 4 7) 1 :add-octave t)  --> (4 7 12 16)"
    (cond ((plusp degree)
    	   (dotimes (i degree)
    	     (setf template (rotate-right template))))
    	  ((minusp degree)
    	   (dotimes (i (abs degree))
    	     (setf template (rotate-left template))))
    	  (t nil))
    (if (not (zerop add-octave))
	(let ((head (car template)))
	  (if (and (not (rest-p head))(< head 116))
	      (setf template (append template (list (+ (* 12 add-octave) head)))))))
    template))


