;;;; CYCO plugins sj eastwest sd3 bigdrums dragon.lisp
;;;;
;;;;
;;;; ew-sd3-bigdrums
;;;;  |
;;;;  +-- dragon
;;;;        variations HITS-1 HITS-2 ROLLS-1 ROLLS-2
;;;;


(param dragon nil)

(let ((keymap-table (make-hash-table))
      (remarks-table (make-hash-table)))
  (setf (gethash 'hits-1 keymap-table)
	(circular-keynumber-map (ew-keynumber 'c1)
				(ew-keynumber 'e6)))
  (setf (gethash 'hits-2 keymap-table)
	(circular-keynumber-map (ew-keynumber 'c1)
				(ew-keynumber 'a4)))
  (setf (gethash 'rolls-1 keymap-table)
	(circular-keynumber-map (ew-keynumber 'c1)
				(ew-keynumber 'a6)))
  (setf (gethash 'rolls-1 keymap-table)
	(circular-keynumber-map (ew-keynumber 'c1)
				(ew-keynumber 'd3)))
  (setf (gethash 'hits-1 remarks-table)
	"SD3 Big Drums  Dragon Ens Hits 1 fullmix, No RR")
  (setf (gethash 'hits-2 remarks-table)
	"SD3 Big Drums  Dragon Ens Hits 2 fullmix, No RR")
  (setf (gethash 'rolls-1 remarks-table)
	"SD3 Big Drums  Dragon Ens Rolls 1 fullmix, NO RR")
  (setf (gethash 'rolls-2 remarks-table)
	"SD3 Big Drums  Dragon Ens Rolls 2 fullmix, NO RR")
  (sd3-instrument dragon ew-sd3-bigdrums keymap-table remarks-table)
  (sd3-query ?dragon 'dragon remarks-table))
