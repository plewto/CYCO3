;;;; CYCO
;;;; Symbolic representation of MIDI key numbers.
;;;;

(in-package :cyco)

(constant +pitch-classes+ #(C CS D DS E F FS G GS A AS B))

(global *KEYNUMBER-TABLE*
	(flet ((make-key-symbol (a b)
				(intern (sformat "~A~A" a b))))
	  (let* ((alternate-key-names #((C BF)(CS DF)(D D)(DS EF)
					(E FF)(F ES)(FS GF)(G G)
					(GS AF)(A A)(AS BF)(B CF)))
		 (key-table (make-hash-table :size 215)))
	    (dotimes (key-number 128)
	      (let* ((pitch-class (rem key-number 12))
		     (octave (truncate (/ key-number 12)))
		     (key-symbols (aref alternate-key-names pitch-class))
		     (primary (make-key-symbol (first key-symbols) octave))
		     (secondary (make-key-symbol (second key-symbols) octave)))
		(setf (gethash primary key-table) key-number)
		(setf (gethash secondary key-table) key-number)))
	    (setf (gethash 'R key-table) +REST+)
	    ;; Simple key numbers sans-octaves
	    (dotimes (i (length +pitch-classes+))
	      (setf (gethash (aref +pitch-classes+ i) key-table) i))
	    key-table)))

(constant +reverse-keynumber-array+
	  (let ((key-name-array (make-array 128 :element-type 'symbol :initial-element nil)))
	    (dotimes (key-number 128)
	      (let* ((pitch-class (rem key-number 12))
		     (octave (truncate (/ key-number 12)))
		     (key-symbol (intern (format nil "~A~A" (aref +pitch-classes+ pitch-class) octave))))
		(setf (aref key-name-array key-number) key-symbol)))
	    key-name-array))

(defun defkeynumber (key-symbol key-number)
  "Define new symbolic keynumber.
key-number - an integer in interval (-1..127) inclusive."
  (setf (gethash key-symbol *keynumber-table*) key-number))
  
(defmethod keynumber-p ((n integer)) t)

 (defmethod rest-p ((object t))
      (and (keynumber-p object)
	   (or (eq object 'r)
	       (and (numberp object)(minusp object)))))

(defmethod keynumber ((object t))
  (cyco-type-error 'keynumber '(integer symbol list) object))

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
      (gethash (resolve-symbol s) *keynumber-table*))
    
    (defmethod keynumber ((key-symbol symbol))
      (let ((symbol (resolve-symbol key-symbol)))
	(or (gethash symbol *keynumber-table*)
	    (cyco-value-error 'keynumber symbol))))

    (defmethod rest-p ((key-symbol symbol))
      (eq 'r (resolve-symbol key-symbol)))))
    
(defmethod keynumber ((key-number-list list))
  (mapcar #'keynumber key-number-list))

(defmethod pitch-class ((object t))
  (cyco-type-error 'pitch-class '(integer symbol list) object))

(defmethod pitch-class ((key-number integer))
  (if (minusp key-number)
      +REST+
    (rem key-number 12)))

(defmethod pitch-class ((key-symbol symbol))
  (if (keynumber-p key-symbol)
      (pitch-class (keynumber key-symbol))
    (cyco-value-error 'pitch-class key-symbol)))

(defmethod pitch-class ((key-number-list list))
  (mapcar #'pitch-class key-number-list))

(defmethod octave ((object t))
  (cyco-type-error 'octave '(number symbol list) object))

(defmethod octave ((key-number number))
  (if (minusp key-number)
      +rest+
    (truncate (/ key-number 12))))

(defmethod octave ((key-symbol symbol))
  (if (keynumber-p key-symbol)
      (octave (keynumber key-symbol))
    (cyco-value-error 'octave key-symbol)))

(defmethod octave ((key-number-list list))
  (mapcar #'octave key-number-list))

(defmethod keyname ((object t))
  (cyco-type-error 'keyname '(integer symbol list) object))

(defmethod keyname ((key-symbol symbol))
  (or (and (keynumber-p key-symbol) key-symbol)
      (cyco-value-error 'keyname key-symbol)))

(defmethod keyname ((key-number integer))
  (let ((kn (keynumber key-number)))
    (if (minusp kn)
	'R
      (aref +reverse-keynumber-array+ kn))))

(defmethod keyname ((key-number-list list))
  (mapcar #'keyname key-number-list))

(defmethod transpose ((key-number integer)(amount integer))
  (if (rest-p key-number)
      +rest+
    (let ((n2 (+ key-number amount)))
      (while (< n2 0)(setf n2 (+ n2 12)))
      (while (> n2 127)(setf n2 (- n2 12)))
      n2)))

(defmethod transpose ((key-symbol symbol)(amount integer))
  (if (keynumber-p key-symbol)
      (keyname (transpose (keynumber key-symbol) amount))
    key-symbol))

(defmethod transpose ((key-number-list list)(amount integer))
  (mapcar #'(lambda (q)(transpose q amount)) key-number-list))

(defmethod invert ((key-number integer)(pivot-key t))
  (if pivot-key
      (let ((pivot-point (keynumber pivot-key)))
	(if (minusp key-number)
	    +rest+
	  (let* ((diff (- pivot-point key-number))
		 (rs (+ pivot-point diff)))
	    rs)))
    key-number))

(defmethod invert ((key-symbol symbol)(pivot-key t))
  (if (and pivot-key (keynumber-p key-symbol))
      (invert (keynumber key-symbol) pivot-key)
    key-symbol))

(defmethod invert ((key-number-list list)(pivot-key t))
  (mapcar #'(lambda (q)(invert q pivot-key)) key-number-list))


(defun white-key-p (object)
  "Predicate, returns true if object is a 'white' key-number."
  (and (keynumber-p object)
       (member (pitch-class object)
	       '(0 2 4 5 7 9 11))))

(defun black-key-p (object)
  "Predicate, returns true if object is a 'black' key-number."
  (and (keynumber-p object)
       (member (pitch-class object)
	       '(1 3 6 8 10))))

(defun white-keys (start-key end-key)
  "Returns list of all 'white' keys betweeen start and end (inclusive)."
  (remove-if-not #'white-key-p (range (keynumber start-key)(1+ (keynumber end-key)))))

(defun black-keys (start-key end-key)
  "Returns list of all 'black' keys between start and end (inclusive)."
  (remove-if-not #'black-key-p (range (keynumber start-key)(1+ (keynumber end-key)))))
