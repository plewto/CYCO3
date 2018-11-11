;;;; CYCO chords/fretworks/chord-family
;;;;

;;;; ---------------------------------------------------------------------- 
;;;;			 %CHORD-VARIATIONS% class
;;;;
;;;; "Helper" class stores variations for a single pitch-class of some
;;;; chord type.  Varations are saved as nested list of MIDI keynumbers.
;;;;
;;;;                    (((...)(...) ... (...))
;;;;

(defclass %chord-variations% nil
  ((vlist
    :type list
    :accessor chord-variation-list
    :initform '()
    :initarg :vlist)))

(defmethod %add-chord-variation% ((cv %chord-variations%)(template list))
  (let ((vlist (reverse (chord-variation-list cv))))
    (push template vlist)
    (setf (chord-variation-list cv)(reverse vlist))))

(defmethod %get-chord-variation% ((cv %chord-variations%)(key integer))
  (let ((vlist (chord-variation-list cv)))
    (cond ((minusp key)
	   +REST+)
	  (t (nth (min key (1- (length vlist))) vlist)))))

(defmethod %get-chord-variation% ((cv %chord-variations%)(key symbol))
  (let ((vlist (chord-variation-list cv))
	(oct (octave key)))
    (cond ((null oct)
	   +REST+)
	  ((>= oct (length vlist))
	   (final vlist))
	  (t
	   (nth oct vlist)))))

(defmethod clone ((cv %chord-variations%) &key new-name new-parent)
  (dismiss new-name new-parent)
  (make-instance '%chord-variations% :vlist (clone (chord-variation-list cv))))

;; sorts variations by average key number, ignoring rest.
;;
(defmethod %sort-chord-variations% ((cv %chord-variations%))
  (flet ((mean (lst)
	       (setf lst (remove -1 lst))
	       (let ((sum (apply #'+ lst))
		     (cnt (length lst)))
		 (/ (float sum) cnt))))
    (let ((acc (sort (chord-variation-list cv)
		     #'(lambda (a b)(< (mean a)(mean b))))))
      (setf (chord-variation-list cv) acc)
      acc)))


;;;; ---------------------------------------------------------------------- 
;;;;			    CHORD-FAMILY class
;;;;
;;;; A CHORD-FAMILY holds all variations for all pitch-classes of given 
;;;; chord-type. 
;;;;

(defclass chord-family nil
  ((chord-type
    :type symbol
    :reader chord-type
    :initform nil
    :initarg :chord-type)
   (key-array
    :type vector
    :reader key-array
    :initform (->vector
	       (copies 12 (make-instance '%chord-variations%) :clone)))))

(defmethod add-chord-family-variation! ((cf chord-family)(key t)(template list))
  (let ((cv (aref (key-array cf)(pitch-class key))))
    (%add-chord-variation% cv template)))

(defmethod get-chord-variation ((cf chord-family)(key t))
  (let* ((pc (pitch-class key))
	 (oct (octave key)))
    (if oct
	(let ((cv (aref (key-array cf) pc)))
	  (%get-chord-variation% cv oct))
      +REST+)))

;; Sorts chord variations by average keynumber, ignoring rest.
(defmethod sort-chord-family! ((cf chord-family))
  (let ((kary (key-array cf)))
    (dotimes (k 12)
      (let ((cvar (aref kary k)))
	(%sort-chord-variations% cvar)))))

;; (defmethod ? ((cf chord-family))
;;   (let ((kary (key-array cf)))
;;     (format t "CHORD-FAMILY ~A~%" (chord-type cf))
;;     (dotimes (k 12)
;;       (format t "  key ~A~%" (pitch-class k))
;;       (let ((cvar (aref kary k)))
;; 	(dolist (vl (chord-variation-list cvar))
;; 	  (let* ((notes (remove -1 vl))
;; 		 (mu (apply #'mean notes)))
;; 	    (format t "  key ~2A  ~20A   mu ~4,2f~%" k notes mu)))))))
