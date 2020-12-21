;;;; CYCO chords  chord-model.lisp
;;;;
;;;; Defines structures for generating chords.
;;;;

(in-package :cyco)

;; Generic methods on chord models
;;    name model
;;    chord-description model name
;;    absolute-chords-p model
;;    defines-chord-p model ctype
;;    chord-types model --> list
;;    dump-chords
;;    chord-template  model name keynumber

(defclass abstract-chord-model nil
  ((name
    :type symbol
    :accessor name
    :initform 'chord-model
    :initarg :name)
   (descriptions			; descriptions of
    :type hash-table			; chord types
    :reader chord-table-descriptions
    :initform (make-hash-table :size 36)
    :initarg :descriptions)
   (is-absolute				; chords are either absolute
    :type t				; list of keynumbers or
    :reader absolute-chords-p		; relative list of keynumber
    :initform nil			; offset.
    :initarg :absolute)))
    
(defmethod chord-model-p ((object abstract-chord-model)) t)

(defmethod defines-chord-p ((chord-model abstract-chord-model)(ctype t))
  (error (sformat "DEFINES-CHORD-P not implemented for ~A" (type-of chord-model))))

(defmethod dump-chords ((chord-model abstract-chord-model))
  (error (sformat "DUMP-CHORDS not implemented for ~A" (type-of chord-model))))

(defmethod chord-template ((chord-model abstract-chord-model)(name t)(keynumber t))
  (declare (ignore keynumber))
  (error (sformat "CHORD-TEMPLATE not implemented for ~A" (type-of chord-model))))

(defmethod chord-description ((chord-model abstract-chord-model)(chord-name t))
  (gethash chord-name (chord-table-descriptions chord-model)))

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
  (defun chord-inversion (template degree &key (add-octave 0))
    "Apply inversion to chord template.
template - A list of keynumber offsets.
degree   - inversion degree.
:add-octave - flag, if true repeat initial note (after inversion)
one octave higher at the end the template list.

(chord-inversion '(0 4 7)  0)    --> (0 4 7)    major triad in root position
(chord-inversion '(0 4 7)  1)    --> (4 7 12)   first inversion
(chord-inversion '(0 4 7)  2)    --> (7 12 16)  second inversion
(chord-inversion '(0 4 7)  4)    --> (12 16 19) 
(chord-inversion '(0 4 7) -1)    --> (-5 0 4)
(chord-inversion '(0 4 7) 0 :add-octave t)  --> (0 4 7 12)
(chord-inversion '(0 4 7) 1 :add-octave t)  --> (4 7 12 16)"
    (cond
     ((null template)
      (return-from chord-inversion '()))
     ((plusp degree)
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
