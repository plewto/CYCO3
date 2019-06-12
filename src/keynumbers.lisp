;;;; CYCO
;;;; Symbolic representation of MIDI key numbers.
;;;;

(constant +PITCH-CLASSES+ #(C CS D DS E F FS G GS A AS B))

(global *KEYNUMBERS*
	  (flet ((make-sym (a b)
			   (intern (sformat "~A~A" a b))))
	    (let* ((symlist #((C BF)(CS DF)(D D)(DS EF)
			      (E FF)(F ES)(FS GF)(G G)
			      (GS AF)(A A)(AS BF)(B CF)))
		   (acc (make-hash-table :size 215)))
	      (dotimes (keynum 128)
		(let* ((pclass (rem keynum 12))
		       (octave (truncate (/ keynum 12)))
		       (syms (aref symlist pclass))
		       (primary (make-sym (first syms) octave))
		       (secondary (make-sym (second syms) octave)))
		  (setf (gethash primary acc) keynum)
		  (setf (gethash secondary acc) keynum)))
	      (setf (gethash 'R acc) +REST+)
	      ;; Simple keynumbers sans-octaves
	      (dotimes (i (length +PITCH-CLASSES+))
		(setf (gethash (aref +PITCH-CLASSES+ i) acc) i))
	      acc)))

(constant +REVERSE-KEYNUMBERS+
	  (let* ((symlist #(C CS D DS E F FS G GS A AS B))
		 (ary (make-array 128 :element-type 'symbol :initial-element nil)))
	    (dotimes (keynum 128)
	      (let* ((pclass (rem keynum 12))
		     (octave (truncate (/ keynum 12)))
		     (sym (intern (format nil "~A~A" (aref symlist pclass) octave))))
		(setf (aref ary keynum) sym)))
	    ary))

(defun defkeynumber (sym value)
  "Define new symbolic keynumber.
sym - symbol
value - an integer in interval (-1..127) inclusive."
  (setf (gethash sym *keynumbers*) value))
  
(defmethod keynumber-p ((n integer)) t)

 (defmethod rest-p ((obj t))
      (and (keynumber-p obj)
	   (or (eq obj 'r)
	       (and (numberp obj)(minusp obj)))))

(defmethod keynumber ((obj t))
  (cyco-type-error 'keynumber '(integer symbol list) obj))

(defmethod keynumber ((n integer))
  (cond ((minusp n) +REST+)
	((< n 128) n)
	(t (while (> n 127)
	     (setf n (- n 12)))
	   n)))

(let ((cyco-package (find-package :cyco)))
  ;; If symbol in foreign package intern in :CYCO 
  (labels ((get-symbol-other-package (sym)
				     (->symbol (symbol-name sym) :cyco))
	   (resolve-symbol (sym)
			   (if (eq (symbol-package sym) cyco-package)
			       sym
			     (get-symbol-other-package sym))))

    (defmethod keynumber-p ((s symbol))
      (gethash (resolve-symbol s) *keynumbers*))
    
    (defmethod keynumber ((s symbol))
      (let ((sym (resolve-symbol s)))
	(or (gethash sym *keynumbers*)
	    (cyco-value-error 'keynumber sym))))

    (defmethod rest-p ((s symbol))
      (eq 'r (resolve-symbol s)))
    ))
    
(defmethod keynumber ((lst list))
  (mapcar #'keynumber lst))

(defmethod pitch-class ((obj t))
  (cyco-type-error 'pitch-class '(integer symbol list) obj))

(defmethod pitch-class ((n integer))
  (if (minusp n)
      +REST+
    (rem n 12)))

(defmethod pitch-class ((s symbol))
  (if (keynumber-p s)
      (pitch-class (keynumber s))
    (cyco-value-error 'pitch-class s)))

(defmethod pitch-class ((lst list))
  (mapcar #'pitch-class lst))

(defmethod octave ((obj t))
  (cyco-type-error 'octave '(number symbol list) obj))

(defmethod octave ((n number))
  (if (minusp n)
      +rest+
    (truncate (/ n 12))))

(defmethod octave ((s symbol))
  (if (keynumber-p s)
      (octave (keynumber s))
    (cyco-value-error 'octave s)))

(defmethod octave ((lst list))
  (mapcar #'octave lst))

(defmethod keyname ((obj t))
  (cyco-type-error 'keyname '(integer symbol list) obj))

(defmethod keyname ((s symbol))
  (or (and (keynumber-p s) s)
      (cyco-value-error 'keyname s)))

(defmethod keyname ((n integer))
  (let ((kn (keynumber n)))
    (if (minusp kn)
	'R
      (aref +reverse-keynumbers+ kn))))

(defmethod keyname ((lst list))
  (mapcar #'keyname lst))

(defmethod transpose ((n integer)(x integer))
  (if (rest-p n)
      +rest+
    (let ((n2 (+ n x)))
      (while (< n2 0)(setf n2 (+ n2 12)))
      (while (> n2 127)(setf n2 (- n2 12)))
      n2)))

(defmethod transpose ((sym symbol)(x integer))
  (if (keynumber-p sym)
      (keyname (transpose (keynumber sym) x))
    sym))

(defmethod transpose ((lst list)(x integer))
  (mapcar #'(lambda (q)(transpose q x)) lst))

(defmethod invert ((n integer)(pivot t))
  (if pivot
      (let ((pp (keynumber pivot)))
	(if (minusp n)
	    +rest+
	  (let* ((diff (- pp n))
		 (rs (+ pp diff)))
	    rs)))
    n))

(defmethod invert ((s symbol)(pivot t))
  (if (and pivot (keynumber-p s))
      (invert (keynumber s) pivot)
    s))

(defmethod invert ((lst list)(pivot t))
  (mapcar #'(lambda (q)(invert q pivot)) lst))


(defun white-keys (start end)
  "Returns list of white-key numbers between start and end.
If either start or end is not a white-key, it is coerced to be so."
  (let ((pc (pitch-class start))
	(s (keynumber start))
	(e (keynumber end)))
    (if (> s e)
	nil
      (cons (if (member pc '(1 3 6 8 10))
		(1+ s)
	      s)
	    (cond ((or (= pc 4)(= pc 11)) ;; E or B
		   (white-keys (1+ s) e))
		  ((member pc '(0 2 5 7 9))
		   (white-keys (+ 2 s) e))
		  (t (white-keys (1+ s) e)))))))


(defun black-keys (start end)
  "Returns list of black key numbers between start and end.
If either start or end is not a black-key, it is coerced to be so."
  (let ((pc (pitch-class start))
	(s (keynumber start))
	(e (keynumber end)))
    (if (> s e)
	nil
      (cons (if (member pc '(0 2 4 5 7 9 11))
		(1+ s)
	      s)
	    (cond ((or (= pc 3)(= pc 10)) ;; DS or AS
		   (black-keys (+ 3 s) e))
		  ((member pc '(1 6 8 10))
		   (black-keys (+ 2 s) e))
		  (t (black-keys (1+ s) e)))))))
