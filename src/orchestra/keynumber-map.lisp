;;;; CYCO orchestra keynumber-map.lisp
;;;;
;;;; Functions to map elements to MIDI keynumbers.
;;;;
;;;;  basic-keynumber-map
;;;;     The default mapping with optional key range and transposition.
;;;;
;;;;  circular-keynumber-map
;;;;     Defined list of keynumber are accessed cyclically.
;;;;     klist = (W X Y)
;;;;     klist[0] --> W
;;;;     klist[1] --> X
;;;;     klist[2] --> Y
;;;;     klist[3] --> W
;;;;
;;;; symbolic-keynumber-map
;;;;   Defined symbols map to MIDI keynumbers, useful for use with percussion synths.
;;;;   kmap('kick)  --> 36
;;;;   kmap('snare) --> 42
;;;;
;;;;   With numeric arguments symbolic-keynumber-map acts like a circular map.
;;;;   kmap(0) --> 36
;;;;   kmap(1) --> 42
;;;;   kmap(2) --> 36
;;;;     
;;;; metronome-keynumber-map
;;;;   Highly specialized symbolic map for use with metronomes.
;;;;
;;;;

(in-package :cyco)

(defun basic-keynumber-map (&key (min 0)(max 127)(transpose 0))
  "Creates a basic keynumber-map
Keynumbers outside range (min max) return +REST+ 
The transpose amount is applied after the key range test."
  (flet ((docfn ()
		(format t ";; Basic keynumber map,  Range [~3D,~3D] transpose ~D.~%"
			min max transpose)
		+rest+)
	 (warnfn (kn)
		 (progn 
		   (cyco-keynumber-warning (sformat "BASIC-KEYNUMBER-MAP  unknown keynumber ~A" kn))
		   +rest+)))
    #'(lambda (kn)
	(cond ((eq kn :doc)
	       (docfn))
	      ((keynumber-p kn)
	       (let ((kn2 (keynumber kn)))
		 (if (minusp kn2)
		     +rest+
		   (progn
		     (setf kn2 (transpose kn2 transpose))
		     (or (and (<= min kn2)(<= kn2 max) kn2)
			 +rest+)))))
	      (t (warnfn kn))))))


(constant +default-keynumber-map+ (basic-keynumber-map))


(defun wrapping-keynumber-map (&key (min 0)(max 127)(transpose 0))
  "Similar to basic-keynumber-map but transposes out of bounds values as needed.
The transpose parameter is applied prior to the range-test."
  (flet ((docfn ()
		(format t ";; WRAPPING-KEYNUMBER-MAP Range [~3D,~3D] transpose ~D.~%"
			min max transpose)
		+rest+))
    #'(lambda (kn)
	(cond ((eq kn :doc)
	       (docfn))
	      ((keynumber-p kn)
	       (let ((kn2 (keynumber kn)))
		 (if (minusp kn2)
		     +rest+
		   (progn
		     (setf kn2 (transpose kn2 transpose))
		     (while (< kn2 min)(setf kn2 (+ kn2 12)))
		     (while (> kn2 max)(setf kn2 (- kn2 12)))
		     kn2))))))))


(defun circular-keynumber-map (start end)
  "Creates a circular keynumber map.
Keynumbers outside range (start end) are reflected back into the range."
  (flet ((docfn ()
		(format t ";; CIRCULAR-KEYNUMBER-MAP range (~A ~A)~%" start end)
		+rest+))
    (let* ((kn1 (keynumber start))
	   (kn2 (keynumber end))
	   (offset (min kn1 kn2))
	   (delta (1+ (- (max kn1 kn2) offset)))
	   (fn #'(lambda (kn)
		   (cond ((eq kn :doc)
			  (docfn))
			 ((rest-p kn)
			  +rest+)
			 (t (let ((k (keynumber kn)))
			      (+ offset (rem k delta))))))))
      fn)))

(defun circular-list-keynumber-map (key-list)
  "Creates circular keynumber map over list of keynumbers."
  (flet ((docfn ()
		(format t ";; CIRCULAR-LIST-KEYNUMBER-MAP ~A~%" key-list)
		+rest+))
    #'(lambda (kn)
	(cond ((eq kn :doc)
	       (docfn))
	      ((rest-p kn)
	       +rest+)
	      (t (cnth kn key-list))))))

(defun finite-list-keynumber-map (key-list)
  "Indexes into list for keynumber.
Out of bounds indexes return a +REST+"
  (flet ((docfn ()
		(format t ";; FINITE-LIST-KEYNUMBER-MAP ~A~%" key-list)
		+rest+))
    (let ((key-vector (->vector key-list))
	  (limit (length key-list)))
      #'(lambda (kn)
	  (cond ((eq kn :doc)(docfn))
		((rest-p kn) +rest+)
		((and (integerp kn)(>= kn 0)(< kn limit))
		 (aref key-vector kn))
		(t +rest+))))))


(defun symbolic-keynumber-map (assignments)
  "Creates a symbolic keynumber-map.
Symbolic maps are most useful with percussion instruments.
The assignments list has the form  ((sym1 . keynumber1)
                                    (sym2 . keynumber2)
                                     ..................)

For integer arguments the map functions as with circular-list-keynumber-map
The spacial symbol 'x returns the first keynumber in the list."
  (flet ((docfn ()
		(format t ";; Symbolic keynumber map~%")
		(dolist (p assignments)
		  (format t ";;   [~16A] --> ~3D~%" (car p)(cdr p)))
		+rest+)
	 (warnfn (kn)
		 (cyco-keynumber-warning (sformat "Unknown keynumber ~A" kn))
		 +rest+))
    (let* ((htab (alist->hash-table assignments (length assignments))))
      #'(lambda (kn)
	  (cond ((eq kn :doc)
		 (docfn)
		 +rest+)
		((rest-p kn)
		 +rest+)
		((eq kn 'x)
		 (keynumber (second (car assignments))))
		((integerp kn)
		 (keynumber (second (cnth kn assignments))))
		(t (let ((assignment (gethash kn htab)))
		     (if assignment
			 (keynumber (car assignment))
		       (warnfn kn)))))))))

		
(defun metronome-keynumber-map (&key (phrase 72)(bar 67)(beat 60))
  "Creates specialized symbolic keynumber-map for metronomes.
The map defines three event types:
:PHRASE - A strong accent on the first beat of the phrase.
:BAR    - A strong accent on the first beat of each bar, except the first.
:BEAT   - all other beats."
  (let ((ktab (make-hash-table :size 3)))
    (setf (gethash :phrase ktab)(keynumber phrase))
    (setf (gethash :bar ktab)(keynumber bar))
    (setf (gethash :beat ktab)(keynumber beat))
    (flet ((docfn ()
		  (format t ";; Metronome keynumber map~%")
		  (format t ";;   :PHRASE  --> ~3D~%" (gethash 'phrase ktab))
		  (format t ";;   :BAR     --> ~3D~%" (gethash 'bar ktab))
		  (format t ";;   :BEAT    --> ~3D~%" (gethash 'beat ktab))
		  +rest+))
      (let ((fn #'(lambda (kn)
		    (if (eq kn :doc)
			(docfn)
		      (or (gethash kn ktab) +rest+)))))
	fn))))
