;;;; PigIron CYCO chords chord-model
;;;;

(defgeneric chord-model-p (obj))
(defgeneric defines-chord-p (model name))
(defgeneric chord-template (model name))
(defgeneric chord-types (model))
(defgeneric define-chord (model name template &optional description))
(defgeneric dump-chords (model))

(defclass chord-model nil nil)

(defmethod chord-model-p ((obj t)) nil)
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
  (defun chord-inversion (key-list degree &key add-octave)
    (cond ((plusp degree)
    	   (dotimes (i degree)
    	     (setf key-list (rotate-right key-list))))
    	  ((minusp degree)
    	   (dotimes (i (abs degree))
    	     (setf key-list (rotate-left key-list))))
    	  (t nil))
    (if (not (zerop add-octave))
	(let ((head (car key-list)))
	  (if (and (not (rest-p head))(< head 116))
	      (setf key-list (append key-list (list (+ (* 12 add-octave) head)))))))
    key-list))
