;;;; PigIron cyco orchestra keynumber-map
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

(defun basic-keynumber-map (&key (min 0)(max 127)(transpose 0)(instrument-name nil))
  (dismiss instrument-name)
  (flet ((docfn ()
		(format t "Basic keynumber map,  Range [~3D,~3D] transpose ~D.~%"
			min max transpose)
		+rest+)
	 (warnfn (kn)
		 (let ((msg (sformat "Unknown keynumber ~A passed to instrument ~A"
				     kn instrument-name)))
		       (cyco-warning msg))
		 +rest+))
    (let ((fn #'(lambda (kn)
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
      fn)))

(constant +default-keynumber-map+ (basic-keynumber-map))


(defun circular-keynumber-map (start end &key instrument-name)
  (flet ((docfn ()
		(format t "Circular keynumber map ~A range (~A ~A)~%"
			(if instrument-name instrument-name "")
			start end)
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


;; assignments an alist ((sym . keynum) ...)
;;
(defun symbolic-keynumber-map (assignments &key instrument-name)
  (flet ((docfn ()
		(format t "Symbolic keynumber map~%")
		(dolist (p assignments)
		  (format t "    [~16A] --> ~3D~%" (car p)(cdr p)))
		+rest+)
	 (warnfn (kn)
		 (cyco-warning (sformat "Unknown keynumber ~A, instrument ~A"
					kn instrument-name))
		 +rest+))
    (let* ((htab (alist->hash-table assignments (length assignments)))
	   (fn #'(lambda (kn)
		   (cond ((eq kn :doc)
			  (docfn)
			  +rest+)
			 ((rest-p kn)
			  +rest+)
			 ((integerp kn)
			  (keynumber (cdr (cnth kn assignments))))
			 (t (or (gethash kn htab)
				(warnfn kn)))))))
      fn)))


(defun metronome-keynumber-map (&key (phrase 72)(bar 67)(beat 60))
  (let ((ktab (make-hash-table :size 3)))
    (setf (gethash 'phrase ktab)(keynumber phrase))
    (setf (gethash 'bar ktab)(keynumber bar))
    (setf (gethash 'beat ktab)(keynumber beat))
    (flet ((docfn ()
		  (format t "Metronome keynumber map~%")
		  (format t "   phrase  -> ~3D~%" (gethash 'phrase ktab))
		  (format t "   bar     -> ~3D~%" (gethash 'bar ktab))
		  (format t "   beat    -> ~3D~%" (gethash 'beat ktab))
		  +rest+))
      (let ((fn #'(lambda (kn)
		    (if (eq kn :doc)
			(docfn)
		      (or (gethash kn ktab) +rest+)))))
	fn))))
